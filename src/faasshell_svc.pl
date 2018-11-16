#!/usr/bin/env swipl
%% -*- mode: prolog; coding: utf-8; -*-
%%
%% Copyright 2017 FUJITSU LIMITED
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%

%%
%%  FaaS Shell Microservice
%%

:- module(faasshell_svc, [main/0]).

:- use_module(faasshell_version, [git_commit_id/1]).
:- use_module(asl_compile, [gen_dsl/2]).
:- use_module(faasshell_run, [start/6]).
:- use_module(cdb_api).
:- use_module(mq_utils).

%% http server
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_log)).
:- use_module(library(http/http_client)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_authenticate)).
:- use_module(library(http/http_cors)).
:- use_module(library(http/http_error)). % should be removed in puroduction

/*******************************
 *   PLUGIN FOR FaaS API       *
 *******************************/
:- multifile
       faas:list/3.

%%
%% setup top directory
%%
:- multifile
       user:file_search_path/2.
:- dynamic
       user:file_search_path/2.

:- prolog_load_context(directory, Dir),
   atomic_concat(Dir, '/..', TopDir),
   asserta(user:file_search_path(faasshell, TopDir)).

user:file_search_path(config_https, faasshell('etc/server')).

%% start
:- current_prolog_flag(emacs_inferior_process, true)
   -> set_prolog_flag(color_term, false)
   ;  set_prolog_flag(verbose, silent),
      initialization(main).

%%
%% main
%%   $ swipl -q -l faasshell_svc.pl -g main -t halt
%%
%% start:
%%   ?- faasshell_svc:main.
%% stop the server:
%%   ?- http_stop_server(8080,[]).
%%
main :-
    set_setting(http:logfile, '/logs/httpd.log'), % docker volume /tmp
    set_setting(http:cors, [*]), % CORS header 'Access-Control-Allow-Origin: *'
    catch( ( mq_utils:mq_init,
             cdb_api:db_init ),
           Error,
           (print_message(error, Error), halt(1))),
    https_options(HttpsOptions),
    ( ( getenv('FAASSHELL_SVC_PORT', PortAtom), PortAtom \== '')
      -> atom_number(PortAtom, PortNum),
         PortOption = [port(PortNum)]
      ;  PortOption = []
    ),
    merge_options(PortOption, HttpsOptions, Options),
    server(Options).

https_options(Options) :-
    getenv('FAASSHELL_SVC_SSL', 'On'),
    https_file('server-key.pem', KeyFile),
    https_file('server-cert.pem', CertFile), !,
    ( https_file(password, PasswordFile)
      ->  read_file_to_string(PasswordFile, Content, []),
          split_string(Content, "", " \n\r", [Password]),
          PasswdOption = [password(Password)]
      ;   PasswdOption = []
    ),
    Options = [port(8443),
               ssl([certificate_file(CertFile), key_file(KeyFile) | PasswdOption])
              ].
https_options([port(8080)]).

https_file(Base, File) :-
    absolute_file_name(config_https(Base), File, [access(read), file_errors(fail)]).

server(Options) :-
    http_server(http_dispatch, Options),
    thread_get_message(stop).

%% signal handler
:- on_signal(hup, _, hup).

hup(_Signal) :-
    thread_send_message(main, stop),
    halt(0).

%%
%%
%:- http_handler('/', base, [methods([get, options]), authentication(faasshell)]).
:- http_handler('/', base, [methods([get, options])]).

base(Request) :-
    http_log('~w~n', [request(Request)]),
    memberchk(method(Method), Request),
    catch( base(Method, Request),
           (Message, Code),
           ( http_log('~w~n', [catch((Message, Code))]),
             reply_json_dict(Message, [status(Code)])
         )).

base(options, Request) :-
    http_log('CORS PreFlight /base~n', []),
    % core_enable/2 has a bug to process headers list
    % https://github.com/SWI-Prolog/packages-http/blob/dcd202477009264481a604d6fcd4867540d014fa/http_cors.pl#L166-L167
    %
    % 162 phrase(field_names(ReqHeaders), String),
    % 167 format('Access-Control-Allow-Headers: ~s', String).
    %                                                ^^^^^^
    % "String" is not string, but code, the fix would be:
    %
    % 162 phrase(field_names(ReqHeaders), Code),
    %     string_code(Code, String),
    % 167 format('Access-Control-Allow-Headers: ~s', String).
    %
    cors_enable(Request,
                [ methods([get]) /*,
                  headers([content_type, authorization]) */
               ]),
    format('~n'). % 200 with empty body

base(get, Request) :-
    cors_enable,
    http:authenticate(faasshell, Request, [faasshell_auth(Id)]),
    ( Id = nil
      -> reply_json_dict(_{error: 'Authentication Failure'}, [status(401)])
      ;  faasshell_version:git_commit_id(Version),
         reply_json_dict(_{version: Version})
    ).

%%
%%
:- http_handler('/executions/', executions, [methods([get]), prefix,
                                             authentication(faasshell)]).

executions(Request) :-
    http_log('~w~n', [request(Request)]),
    cors_enable,
    option(faasshell_auth(nil), Request)
    -> reply_json_dict(_{error: 'Authentication Failure'}, [status(401)])
    ;  memberchk(method(Method), Request),
       catch( executions(Method, Request),
              (Message, Code),
              ( http_log('~w~n', [catch((Message, Code))]),
                reply_json_dict(Message, [status(Code)])
              )).

executions(get, Request) :-
    option(faasshell_auth(NS), Request),
    ( memberchk(path_info(Executions), Request)
      ->  atomic_list_concat([NS, Executions], '/', NSFile),
          doc_read(faasshell_executions, NSFile, Code1, Res),
          ( Code1 = 200
            -> select_dict(_{'_id':_, '_rev':_}, Res, Reply)
            ;  Reply = Res  % {"error":"not_found", "reason":"missing"}
          )
      ;  format(string(Query), '["~w",~w]', [NS, 0]),
         uri_encoded(query_value, Query, EncodedQuery),
         view_read(faasshell_executions, executions, executions,
              ['?startkey=', EncodedQuery], Code, Dict),
         ( Code = 200
           -> Reply = Dict.rows
           ;  Reply = Dict
         )
    ),
    reply_json_dict(Reply).

%%
%%
:- http_handler('/activity/', activity, [methods([get, post, patch]), prefix,
                                         authentication(faasshell)]).

activity(Request) :-
    http_log('~w~n', [request(Request)]),
    cors_enable,
    option(faasshell_auth(nil), Request)
    -> reply_json_dict(_{error: 'Authentication Failure'}, [status(401)])
    ;  memberchk(method(Method), Request),
       catch( activity(Method, Request),
              (Message, Code),
              ( http_log('~w~n', [catch((Message, Code))]),
                reply_json_dict(Message, [status(Code)])
              )).

activity(get, Request) :-
    ( memberchk(path_info(Activity), Request)
      -> uuid(TaskToken),
         http_log('~w, ~p~n',
                  [activity(get, Activity), task_token(TaskToken)]),
         mq_utils:activity_start(Activity, TaskToken, InputText),
         atom_json_dict(InputText, Input, []),
         Reply = _{output: "ok", taskToken: TaskToken, input: Input}
      ;  http_header:status_number(bad_request, S_400),
         throw((_{error: 'Missing activity task name'}, S_400))
    ),
    reply_json_dict(Reply).

activity(post, Request) :-
    http_read_json_dict(Request, Dict, []),
    http_log('~w, ~p~n', [activity(post), params(Dict)]),
    ( _{output: Output, taskToken: TaskToken} :< Dict
      -> Result = success,
         atom_json_dict(OutputText, Output, []),
         http_log('~w, ~p~n', [activity(post(Result)),
                               output_text(OutputText)])
      ; ( _{error: Error, cause: Cause, taskToken: TaskToken} :< Dict
          -> Result = failure,
             atom_json_dict(OutputText, _{error: Error, cause: Cause}, []),
             http_log('~w, ~p~n', [activity(post(Result)),
                               output_text(OutputText)])
          ; http_header:status_number(bad_request, S_400),
            throw((_{error: 'InvalidOutput'}, S_400))
        )
    ),
    ( memberchk(path_info(Activity), Request)
      -> http_log('~w, ~p~n', [activity(post),
                               (Activity, TaskToken, Result, OutputText)]),
         mq_utils:activity_end(Activity, TaskToken, Result, OutputText),
         Reply = _{}
      ;  http_header:status_number(bad_request, S_400),
         throw((_{error: 'Missing activity task name'}, S_400))
    ),
    reply_json_dict(Reply).

activity(patch, Request) :-
    http_read_json_dict(Request, Dict, []),
    http_log('~w, ~p~n', [activity(patch), params(Dict)]),
    ( _{taskToken: TaskToken} :< Dict
      -> true
      ; http_header:status_number(bad_request, S_400),
        throw((_{error: 'InvalidToken'}, S_400))
    ),
    ( memberchk(path_info(Activity), Request)
      -> mq_utils:activity_heartbeat(Activity, TaskToken),
         Reply = _{}
      ;  http_header:status_number(bad_request, S_400),
         throw((_{error: 'Missing activity task name'}, S_400))
    ),
    reply_json_dict(Reply).

%%
%% send event to event state in statemachine
:- http_handler('/trigger/', trigger, [methods([post]), prefix,
                                         authentication(faasshell)]).

trigger(Request) :-
    http_log('~w~n', [request(Request)]),
    cors_enable,
    option(faasshell_auth(nil), Request)
    -> reply_json_dict(_{error: 'Authentication Failure'}, [status(401)])
    ;  memberchk(method(Method), Request),
       catch( trigger(Method, Request),
              (Message, Code),
              ( http_log('~w~n', [catch((Message, Code))]),
                reply_json_dict(Message, [status(Code)])
              )).

trigger(post, Request) :-
    ( memberchk(path_info(Event), Request)
      ->  http_read_json_dict(Request, Dict, []),
          http_log('~w, ~p~n', [trigger(post), params(Dict)]),
          option(faasshell_auth(User), Request),
          ( _{action: Action} :< Dict
            -> Reply = _{}
            ;  http_header:status_number(bad_request, S_400),
               throw((_{error: 'Missing action name'}, S_400))
          ),
          ( _{timeout: Timeout} :< Dict
            -> true
            ;  Timeout = 600
          ),
          ( mq_utils:event_subscribed(User, Event, Timeout)
            -> ( mq_utils:event_publish(User, Event, Action, 600)
                 -> http_log('~w, ~p~n', [trigger(published),
                                          (User, Event, Action)])
                 ; %% critical error
                   %% subscribe message has been removed but queue is full
                   http_header:status_number(bad_request, S_400),
                   throw((_{error: 'Timeout publish event'}, S_400))
               )
            ;  http_header:status_number(bad_request, S_400),
               throw((_{error: 'Timeout subscribe event'}, S_400))
          )
      ;  http_header:status_number(bad_request, S_400),
         throw((_{error: 'Missing event name'}, S_400))
    ),
    reply_json_dict(Reply).

%% $ curl -sLX GET localhost:8080/faas
%% $ curl -sLX GET localhost:8080/faas/{actionName}
:- http_handler('/faas/', faas, [methods([get]), prefix,
                                 authentication(faasshell)]).

faas(Request) :-
    http_log('~w~n', [request(Request)]),
    cors_enable,
    option(faasshell_auth(nil), Request)
    -> reply_json_dict(_{error: 'Authentication Failure'}, [status(401)])
    ;  memberchk(method(Method), Request),
       catch( faas(Method, Request),
              (Message, Code),
              ( http_log('~w~n', [catch((Message, Code))]),
                reply_json_dict(Message, [status(Code)])
              )).

faas(get, Request) :-
    ( memberchk(path_info(Action), Request)
      -> %% Fully-Qualified Action Name
         faas:list(Action, [], Reply)
      ;  findall(R, faas:list([], [], R), Reply)
    ),
    reply_json_dict(Reply).

%%     GET: get statemachine information
%%     PUT: create statemachine
%%    POST: execute statemachine
%%  DELETE: delete statemachine
%%   PATCH: create graph of statemachine
%% OPTIONS: CORS preflight
:- http_handler('/statemachine/', statemachine,
                [methods([get, put, post, delete, patch, options]), prefix]).

statemachine(Request) :-
    http_log('~w~n', [request(Request)]),
    ( memberchk(method(options), Request)
      -> statemachine(options, Request)
      ;  cors_enable,
         http:authenticate(faasshell, Request, FaasshellAuth),
         merge_options(FaasshellAuth, Request, MergedRequest),
         ( option(faasshell_auth(nil), MergedRequest)
           -> reply_json_dict(_{error: 'Authentication Failure'}, [status(401)])
           ;  memberchk(method(Method), MergedRequest),
              catch( statemachine(Method, MergedRequest),
                     (Message, Code),
                     ( http_log('~w~n', [catch((Message, Code))]),
                       reply_json_dict(Message, [status(Code)])
                   ))
         )
    ).

%% CORS prefight
%% $ curl -vX OPTIONS localhost:8080/statemachine/
statemachine(options, Request) :-
    http_log('CORS PreFlight /statemachine~n', []),
    cors_enable(Request, [methods([get, put, post, delete, patch, options])]),
    format('~n'). % 200 with empty body

%% get state machine information
%% $ curl -sLX GET localhost:8080/statemachine/{statemachine}
%% $ curl -sLX GET localhost:8080/statemachine/
statemachine(get, Request) :-
    option(faasshell_auth(NS), Request),
    ( memberchk(path_info(File), Request)
      -> atomics_to_string([NS, "/", File], NSFile),
         cdb_api:doc_read(faasshell, NSFile, Code1, Dict1),
         http_log('~w~n', [doc_read(Code1)]),
         ( Code1 = 200
           -> select_dict(_{'_id':_, '_rev':_}, Dict1, Dict1Rest),
              Output = _{output:ok}.put(Dict1Rest)
           ;  throw((Dict1, Code1))
         )
      ;  format(string(Query), '["asl","~w"]',[NS]),
         uri_encoded(query_value, Query, EncordedQuery),
         cdb_api:view_read(faasshell, faasshell, statemachine,
                           ['?key=', EncordedQuery], Code2, Dict2),
         http_log('~w~n', [view_read(Code2, Dict2)]),
         ( Code2 = 200
           -> maplist([Row,Elm]>>(
                          _{value: [Namespace, Name]} :< Row,
                          Elm = _{namespace: Namespace, name: Name}
                      ), Dict2.rows, Value),
              Output = _{output:ok}.put(asl, Value)
           ;  throw((Dict2, Code2))
         )
    ),
    reply_json_dict(Output).

%% create state machine
%% $ curl -sX PUT localhost:8080/statemachine/{statemachine}?overwrite=true
%%        -H 'Content-Type: application/json' -d @asl.json
statemachine(put, Request) :-
    http_read_json_dict(Request, Dict, []),
    http_log('~w~n', [params(Dict)]),
    ( option(path_info(File), Request),
      option(faasshell_auth(NS), Request)
      -> atomics_to_string([NS, "/", File], NSFile),
         asl_compile:gen_dsl(Dict, Dsl),
         http_log('~w: ~w~n', [NSFile, fsm(Dsl)]),
         term_string(Dsl, DslStr),
         ( Dsl = fsm(_)
           -> AslDict = _{name: File, namespace: NS, asl: Dict, dsl: DslStr},
              http_parameters(Request, [overwrite(Overwrite, [default(false)])]),
              http_log('~w~n', [overwrite(Overwrite)]),
              ( Overwrite = false
                -> cdb_api:doc_create(faasshell, NSFile, AslDict, Code, Res),
                   http_log('~w~n', [doc_create(Code, Res)])
                ;  cdb_api:doc_update(faasshell, NSFile, AslDict, Code, Res),
                   http_log('~w~n', [doc_update(Code, Res)])
              ),
              ( Code = 201
                -> Output = AslDict.put(_{output:ok, dsl: DslStr})
                ;  throw((Res, Code))
              )
           ;  http_header:status_number(server_error, S_500),
              throw((_{error: 'syntax error', reason: DslStr}, S_500))
         )
      ; http_header:status_number(bad_request, S_400),
        throw((_{error: 'Missing statemashine name'}, S_400))
    ),
    reply_json_dict(Output).

%% execute state machine
%% $ curl -sX POST localhost:8080/statemachine/{statemachine} \
%%        -H 'Content-Type: application/json' -d '{"input":{"arg":1}}'
statemachine(post, Request) :-
    ( http_read_json_dict(Request, Params, [])
      -> true
      ;  Params = _{input:_{}}
    ),
    http_log('~w~n', [params(Params)]),
    ( get_dict(input, Params, Input)
      -> true
      ;  http_header:status_number(bad_request, S_400),
         throw((_{error:'Missing input key in params'}, S_400))
    ),
    ( memberchk(path_info(File), Request),
      option(faasshell_auth(NS), Request)
      -> atomics_to_string([NS, "/", File], NSFile),
         cdb_api:doc_read(faasshell, NSFile, Code, Dict),
         http_log('~w~n', [doc_read(File, Code)]),
         select_dict(_{'_id':_, '_rev':_}, Dict, DictRest),
         DictParams = DictRest.put(Params),
         ( Code = 200
           -> % http_log('~w~n', [fsm(Dict.Dsl)]),
              term_string(Dsl, Dict.dsl),
              http_parameters(Request, [blocking(Blocking, [default(false)])]),
              http_log('~w~n', [blocking(Blocking)]),
              ( Blocking = true
                -> ( faasshell_run:start(Dsl, [faasshell_auth(NS)], Input, O,
                                         _{}, _)
                     -> Output = DictParams.put(_{output:O})
                     ;  throw((_{error: 'failed to start',
                                 cause: 'FaaS credential is missing'}, 500))
                   )
               ;  uuid(ExecId),
                  thread_create(background_job(NS, File, ExecId, Dsl, Input),
                                ChildId),
                  http_log('~w~n', [background_job(ChildId)]),
                  Output = DictParams.put(_{output: _{execution_id: ExecId}})
              )
           ;  throw((_{error: 'database error'}, 500))
         )
      ;  http_header:status_number(bad_request, S_400),
         throw((_{error: 'Missing statemashine name'}, S_400))
    ),
    reply_json_dict(Output).

%% delete state machine
%% $ curl -sX DELETE localhost:8080/statemachine/{statemachine}
statemachine(delete, Request) :-
    ( memberchk(path_info(File), Request),
      option(faasshell_auth(NS), Request)
      -> atomics_to_string([NS, "/", File], NSFile),
         cdb_api:doc_delete(faasshell, NSFile, Code, Res),
         http_log('~w~n', [doc_delete(Code, Res)]),
         ( Code = 200
           -> Output = _{output:ok}
           ;  throw((Res, Code))
         )
      ; http_header:status_number(bad_request, S_400),
        throw((_{error: 'Missing statemashine name'}, S_400))
    ),
    reply_json_dict(Output).

%% create graph of state machine
%% $ curl -sX PATTCH localhost:8080/statemachine/{statemachine}
statemachine(patch, Request) :-
    memberchk(path_info(File), Request),
    option(faasshell_auth(NS), Request)
    -> atomics_to_string([NS, "/", File], NSFile),
       cdb_api:doc_read(faasshell, NSFile, Code, Dict),
       http_log('~w~n', [doc_read(Code)]),
       ( Code = 200
         -> format('Content-type: text/plain~n~n'),
            asl_compile:gen_dot(Dict.asl)
         ;  throw((Dict, Code))
       )
    ; http_header:status_number(bad_request, S_400),
      throw((_{error: 'Missing statemashine name'}, S_400)).

%%    GET: get shell.dsl information
%%    PUT: create shell.dsl
%%   POST: execute shell.dsl
%% DELETE: delete shell.dsl
:- http_handler('/shell/', shell,
                [methods([get, put, post, delete]), prefix,
                 authentication(faasshell)]).

shell(Request) :-
    http_log('~w~n', [request(Request)]),
    cors_enable,
    option(faasshell_auth(nil), Request)
    -> reply_json_dict(_{error: 'Authentication Failure'}, [status(401)])
    ;  memberchk(method(Method), Request),
       catch( shell(Method, Request),
              (Message, Code),
              ( http_log('~w~n', [catch((Message, Code))]),
                reply_json_dict(Message, [status(Code)])
              )).

%% get shell information
%% $ curl -sLX GET localhost:8080/shell/{shell.dsl}
%% $ curl -sLX GET localhost:8080/shell
shell(get, Request) :-
    option(faasshell_auth(NS), Request),
    ( memberchk(path_info(File), Request)
      -> atomics_to_string([NS, "/", File], NSFile),
         cdb_api:doc_read(faasshell, NSFile, Code1, Dict1),
         http_log('~w~n', [doc_read(Code1, Dict1)]),
         ( Code1 = 200
           -> select_dict(_{'_id':_, '_rev':_}, Dict1, Dict1Rest),
              Output = _{output:ok}.put(Dict1Rest)
           ;  throw((Dict1, Code1))
         )
      ;  format(string(Query), '["dsl","~w"]',[NS]),
         uri_encoded(query_value, Query, EncordedQuery),
         cdb_api:view_read(faasshell, faasshell, shell,
                           ['?key=', EncordedQuery], Code2, Dict2),
         http_log('~w~n', [view_read(Code2, Dict2)]),
         ( Code2 = 200
           -> maplist([Row,Elm]>>(
                          _{value: [Namespace, Name]} :< Row,
                          Elm = _{namespace: Namespace, name: Name}
                      ), Dict2.rows, Value),
              Output = _{output:ok}.put(dsl, Value)
           ;  throw((Dict2, Code2))
         )
    ),
    reply_json_dict(Output).

%% create shell
%% $ curl -sX PUT localhost:8080/shell/{shell.dsl}?overwrite=true \
%%        -H 'Content-Type: text/plain' -d @shell.dsl
shell(put, Request) :-
    http_read_data(Request, DslStr, [text/plain]),
    http_log('~w~n', [put(Dsl)]),
    ( memberchk(path_info(File), Request),
      option(faasshell_auth(NS), Request)
      -> ( term_string(Dsl, DslStr),
           Dsl = fsm(_)
           -> atomics_to_string([NS, "/", File], NSFile),
              http_parameters(Request, [overwrite(Overwrite, [default(false)])]),
              http_log('~w~n', [overwrite(Overwrite)]),
              Dict = _{dsl: DslStr, name: File, namespace: NS},
              ( Overwrite = false
                -> cdb_api:doc_create(faasshell, NSFile, Dict, Code, Res),
                   http_log('~w~n', [doc_create(Code, Res)])
                ;  cdb_api:doc_update(faasshell, NSFile, Dict, Code, Res),
                   http_log('~w~n', [doc_update(Code, Code, Res)])
              ),
              ( Code = 201
                -> Output = Dict.put(_{output:ok})
                ;  throw((Res, Code))
              )
           ; http_header:status_number(server_error, S_500),
             throw((_{error: 'syntax error', reason: DslStr}, S_500))
         )
      ; http_header:status_number(bad_request, S_400),
        throw((_{error: 'Missing shell name'}, S_400))
    ),
    reply_json_dict(Output).

%% execute shell
%% $ curl -sX POST localhost:8080/shell/{shell.dsl} \
%%        -H 'Content-Type: application/json' -d '{"input":{"arg":1}}'
shell(post, Request) :-
    ( http_read_json_dict(Request, Params, [])
      -> true
      ;  Params = _{input:_{}}
    ),
    http_log('~w~n', [params(Params)]),
    ( get_dict(input, Params, Input)
      -> true
      ;  http_header:status_number(bad_request, S_400),
         throw((_{error: 'Missing input key in params'}, S_400))
    ),
    ( memberchk(path_info(File), Request),
      option(faasshell_auth(NS), Request)
      -> atomics_to_string([NS, "/", File], NSFile),
         cdb_api:doc_read(faasshell, NSFile, Code, Dict),
         http_log('~w~n', [doc_read(NSFile, Code)]),
         ( Code = 200
           -> select_dict(_{'_id':_, '_rev':_}, Dict, DictRest),
              DictParams = DictRest.put(Params),
              % http_log('~w~n', [fsm(Dsl)]),
              term_string(Dsl, Dict.dsl),
              http_parameters(Request, [blocking(Blocking, [default(false)])]),
              http_log('~w~n', [blocking(Blocking)]),
              ( Blocking = true
                -> ( faasshell_run:start(Dsl, [faasshell_auth(NS)], Input, O,
                                         _{}, _)
                     -> Output = DictParams.put(_{output:O})
                     ;  throw((_{error: 'failed to start',
                                 cause: 'FaaS credential is missing'}, 500))
                   )
               ;  uuid(ExecId),
                  thread_create(background_job(NS, File, ExecId, Dsl, Input),
                                ChildId),
                  http_log('~w~n', [background_job(ChildId)]),
                  Output = DictParams.put(_{output: _{execution_id: ExecId}})
              )
           ;  throw((Dict, Code))
         )
      ; http_header:status_number(bad_request, S_400),
        throw((_{error: 'Missing shell name'}, S_400))
    ),
    reply_json_dict(Output).

%% delete shell
%% $ curl -sX DELETE localhost:8080/shell/{shell.dsl}
shell(delete, Request) :-
    ( memberchk(path_info(File), Request),
      option(faasshell_auth(NS), Request)
      -> atomics_to_string([NS, "/", File], NSFile),
         cdb_api:doc_delete(faasshell, NSFile, Code, Res),
         http_log('~w~n', [doc_delete(NSFile, Code, Res)]),
         ( Code = 200
           -> Output = _{output:ok}
           ;  throw((Res, Code))
         )
      ; http_header:status_number(bad_request, S_400),
        throw((_{error: 'Missing shell name'}, S_400))
    ),
    reply_json_dict(Output).

/*******************************
 *   PLUGIN FOR HTTP_DISPATCH   *
 *******************************/
:- multifile
http:authenticate/3.

:- use_module(library(debug)).

%% $ swipl -q -l src/faasshell_svc.pl -g faasshell_svc:debug_auth -g main -t halt
debug_auth :- debug(http_authenticate > user_error).

%%
:- dynamic
       cached_auth/4. % cached_auth(User, Password, Id, Time)

http:authenticate(faasshell, Request, [faasshell_auth(Id)]) :-
    memberchk(authorization(Text), Request)
    -> debug(http_authenticate, 'Authorization: ~w~n', [Text]),
       http_authorization_data(Text, basic(User, PasswordCode)),
       atom_codes(Password, PasswordCode),
       debug(http_authenticate, 'User: ~w, Password: ~s~n', [User, Password]),
       ( ( cached_auth(User, Password, Id, Time),
           get_time(Now),
           Now-Time =< 60
         )
         -> debug(http_authenticate, 'Hit Cache: ~w, ~w~n', [User, Time]),
            http_log('Subject(cache): ~w, ~w~n', [User, Time])
         ;  ( retract(cached_auth(User, Password, Id, Time))
              -> debug(http_authenticate, 'retracted cache: ~w, ~w~n', [User, Time])
              ;  debug(http_authenticate, 'cache not exist: ~w, ~w~n', [User, Time])
            ),
            atomic_list_concat([User, Password], ':', Credential),
            debug(http_authenticate, 'check var: ~w, ~w~n', [Id, Credential]),
            ( cdb_api:get_user(Id, Credential)
              -> get_time(Updated),
                 assertz(cached_auth(User, Password, Id, Updated)),
                 debug(http_authenticate, 'asserted cache: ~w, ~w, ~w~n',
                       [User, Time, Updated]),
                 http_log('Subject(refresh): ~w, ~w-~w~n', [User, Time, Updated])
              ;  http_log('Authentication failed: ~w~n', [User]),
                 Id = nil
            )
       )
    ;  http_log('Authentication failed: Authorization header not found~n', []),
       Id = nil.

%%
%%
background_job(Namespace, File, ExecId, Dsl, Input) :-
    atomic_list_concat([Namespace, ExecId], '/', NSFile),
    get_time(Start),
    gethostname(Hostname),
    Dict = _{ start: Start,
              namespace: Namespace,
              statemachine: File,
              execution_id: ExecId,
              hostname: Hostname
            },
    cdb_api:doc_create(faasshell_executions, NSFile, Dict, _C1, _R1),
    ( faasshell_run:start(Dsl, [faasshell_auth(Namespace)], Input, O, _{}, _)
      -> Output = O
      ;  Output = _{error: 'failed to start',
                    cause: 'FaaS credential is missing'}
    ),
    get_time(End),
    UpdatedDict = Dict.put(
           _{ result:
              _{ input: Input,
                 output: Output
               },
             end: End
            }),
    cdb_api:doc_update(faasshell_executions, NSFile, UpdatedDict, _C2, _R2),
    thread_self(Self),
    thread_detach(Self).
