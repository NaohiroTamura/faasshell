%% #!/usr/bin/swipl -q
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

:- module(asl_svc, [main/0]).

:- use_module(asl_gen, [gen_dsl/2]).
:- use_module(asl_run, [start/4]).
:- use_module(cdb_api).

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
%% :- use_module(library(http/http_error)). % should be removed in puroduction

%% start
%% :- initialization(main).

%%
%% main
%%   $ swipl -q -l asl_svc.pl -g main -t halt
%%
%% start:
%%   ?- asl_svc:main.
%% stop the server:
%%   ?- http_stop_server(8080,[]).
%%
main :-
    set_setting(http:logfile,'/logs/httpd.log'), % docker volume /tmp
    catch(db_init(faasshell, faas, _Codes),
          Error,
          (print_message(error, Error), halt(1))),
    getenv('SVC_PORT', Port) -> server(Port); server(8080).

server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    thread_get_message(stop).

%% signal handler
:- on_signal(hup, _, hup).

hup(_Signal) :-
    thread_send_message(main, stop),
    halt(0).

%% $ curl -sLX GET localhost:8080/faas
%% $ curl -sLX GET localhost:8080/faas/{actionName}
:- http_handler('/faas/', faas, [methods([get]), prefix,
                                 authentication(openwhisk)]).

faas(Request) :-
    http_log('~w~n', [request(Request)]),
    option(api_key(ID-PW), Request),
    wsk_api_utils:openwhisk(Defaults),
    merge_options([api_key(ID-PW)], Defaults, Options),
    ( memberchk(path_info(Action), Request)
      -> %% Fully-Qualified Action Name
         option(namespace(NS), Request),
         atomics_to_string(["/", NS, "/", Action], FQAN),
         wsk_api_actions:list(FQAN, Options, Reply)
      ;  wsk_api_actions:list(none, Options, Reply)
    ),
    reply_json_dict(Reply).

%%    GET: get statemachine information
%%    PUT: create statemachine
%%   POST: execute statemachine
%% DELETE: delete statemachine
%% PATCH : create graph of statemachine
:- http_handler('/statemachine/', statemachine,
                [methods([get, put, post, delete, patch]), prefix,
                 authentication(openwhisk)]).

statemachine(Request) :-
    http_log('~w~n', [request(Request)]),
    memberchk(method(Method), Request),
    statemachine(Method, Request).

%% get state machine information
%% $ curl -sLX GET localhost:8080/statemachine/{statemachine}
%% $ curl -sLX GET localhost:8080/statemachine/
statemachine(get, Request) :-
    ( memberchk(path_info(File), Request),
      option(namespace(NS), Request)
      -> atomics_to_string([NS, "/", File], NS_File),
         cdb_api:doc_read(faasshell, NS_File, Code1, Dict1),
         http_log('~w~n', [doc_read(Code1)]),
         ( Code1 = 200
           -> Output = _{output:ok}.put(Dict1)
           ;  Output = _{output:ng}.put(Dict1)
         )
      ;  cdb_api:view_read(faasshell, faas, statemachine, [], Code2, Dict2),
         http_log('~w~n', [view_read(Code2, Dict2)]),
         ( Code2 = 200
           -> ( [Row0] = Dict2.rows -> Value = Row0.value; Value = null ),
              Output = _{output:ok}.put(asl, Value)
           ;  Output = _{output:ng}.put(Dict2)
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
      option(namespace(NS), Request)
      -> atomics_to_string([NS, "/", File], NS_File),
         asl_gen:gen_dsl(Dict.asl, Dsl),
         http_log('~w: ~w~n', [NS_File, dsl(Dsl)]),
         term_string(Dsl, DslStr),
         ( Dsl = asl(_)
           -> http_parameters(Request, [overwrite(Overwrite, [default(false)])]),
              http_log('~w~n', [overwrite(Overwrite)]),
              ( Overwrite = false
                -> cdb_api:doc_create(faasshell, NS_File, Dict, Code, Res),
                   http_log('~w~n', [doc_create(Code, Res)])
                ;  cdb_api:doc_update(faasshell, NS_File, Dict, Code, Res),
                   http_log('~w~n', [doc_update(Code, Res)])
              ),
              ( Code = 201
                -> Output = Dict.put(_{output:ok, dsl: DslStr})
                ;  Output = Dict.put(_{output:ng}).put(Res)
              )
           ;  Output = Dict.put(_{output:ng, error: "syntax error!", dsl: DslStr})
         )
      ; Output = _{output:ng, error:"statemashine missing!"}
    ),
    reply_json_dict(Output).

%% execute state machine
%% $ curl -sX POST localhost:8080/statemachine/{statemachine} \
%%        -H 'Content-Type: application/json' -d '{"input":{"arg":1}}'
statemachine(post, Request) :-
    ( http_read_json_dict(Request, Params, []); Params = _{} ),
    http_log('~w~n', [params(Params)]),
    ( memberchk(path_info(File), Request),
      option(namespace(NS), Request)
      -> atomics_to_string([NS, "/", File], NS_File),
         cdb_api:doc_read(faasshell, NS_File, Code, Dict),
         http_log('~w~n', [doc_read(File, Code)]),
         ( Code = 200
           -> asl_gen:gen_dsl(Dict.asl, Dsl),
              % http_log('~w~n', [dsl(Dsl)]),
              term_string(Dsl, DslStr),
              ( Dsl = asl(_)
                -> option(api_key(ID-PW), Request),
                   wsk_api_utils:openwhisk(Defaults),
                   merge_options([api_key(ID-PW)], Defaults, Options),
                   Input = Dict.put(Params),
                   asl_run:start(Dsl, Options, Input.input, O),
                   Output = Dict.put(_{output:O})
                ;  Output = Dict.put(_{output:ng, error: DslStr})
              )
           ;  Output = _{output:ng}.put(Dict)
         )
      ;  Output = _{output:ng, error:"statemashine missing!"}
    ),
    reply_json_dict(Output).

%% delete state machine
%% $ curl -sX DELETE localhost:8080/statemachine/{statemachine}
statemachine(delete, Request) :-
    ( memberchk(path_info(File), Request),
      option(namespace(NS), Request)
      -> atomics_to_string([NS, "/", File], NS_File),
         cdb_api:doc_delete(faasshell, NS_File, Code, Res),
         http_log('~w~n', [doc_delete(Code, Res)]),
         ( Code = 200
           -> Output = _{output:ok}
           ;  Output = _{output:ng}.put(Res)
         )
      ; Output = _{output:ng, error:"statemashine missing!"}
    ),
    reply_json_dict(Output).

%% create graph of state machine
%% $ curl -sX PATTCH localhost:8080/statemachine/{statemachine}
statemachine(patch, Request) :-
    memberchk(path_info(File), Request),
    option(namespace(NS), Request)
    -> atomics_to_string([NS, "/", File], NS_File),
       cdb_api:doc_read(faasshell, NS_File, Code, Dict),
       http_log('~w~n', [doc_read(Code)]),
       ( Code = 200
         -> format('Content-type: text/plain~n~n'),
            asl_gen:gen_dot(Dict.asl)
         ;  reply_json_dict(_{output:ng}.put(Dict))
       )
    ; reply_json_dict(_{output:ng, error: "statemachine missing!"}).

%%    GET: get shell.dsl information
%%    PUT: create shell.dsl
%%   POST: execute shell.dsl
%% DELETE: delete shell.dsl
:- http_handler('/shell/', shell,
                [methods([get, put, post, delete]), prefix,
                 authentication(openwhisk)]).

shell(Request) :-
    http_log('~w~n', [request(Request)]),
    memberchk(method(Method), Request),
    shell(Method, Request).

%% get shell information
%% $ curl -sLX GET localhost:8080/shell/{shell.dsl}
%% $ curl -sLX GET localhost:8080/shell
shell(get, Request) :-
    ( memberchk(path_info(File), Request),
      option(namespace(NS), Request)
      -> atomics_to_string([NS, "/", File], NS_File),
         cdb_api:doc_read(faasshell, NS_File, Code1, Dict1),
         http_log('~w~n', [doc_read(Code1, Dict1)]),
         ( Code1 = 200
           -> Output = _{output:ok, dsl: Dict1.dsl}
           ;  Output = _{output:ng}.put(Dict1)
         )
      ;  cdb_api:view_read(faasshell, faas, shell, [], Code2, Dict2),
         http_log('~w~n', [view_read(Code2, Dict2)]),
         ( Code2 = 200
           -> ( [Row0] = Dict2.rows -> Value = Row0.value; Value = null ),
              Output = _{output:ok}.put(asl, Value)
           ;  Output = _{output:ng}.put(Dict2)
         )
    ),
    reply_json_dict(Output).

%% create shell
%% $ curl -sX PUT localhost:8080/shell/{shell.dsl}?overwrite=true \
%%        -H 'Content-Type: text/plain' -d @shell.dsl
shell(put, Request) :-
    http_read_data(Request, DslStr, [text/plain]),
    term_string(Dsl, DslStr),
    http_log('~w~n', [put(Dsl)]),
    ( Dsl = asl(_)
      -> ( memberchk(path_info(File), Request),
           option(namespace(NS), Request)
           -> atomics_to_string([NS, "/", File], NS_File),
              http_parameters(Request, [overwrite(Overwrite, [default(false)])]),
              http_log('~w~n', [overwrite(Overwrite)]),
              ( Overwrite = false
                -> cdb_api:doc_create(faasshell, NS_File, _{dsl:DslStr}, Code, Res),
                   http_log('~w~n', [doc_create(Code, Res)])
                ;  cdb_api:doc_update(faasshell, NS_File, _{dsl:DslStr}, Code, Res),
                   http_log('~w~n', [doc_update(Code, Code, Res)])
              ),
              ( Code = 201
                -> Output = _{output:ok, dsl: DslStr}
                ;  Output = _{output:ng}.put(Res)
              )
           ; Output = _{output:ng, error: "shell name missing!"}
         )
      ; Output = _{output:ng, error: "syntax error!"}
    ),
    reply_json_dict(Output).

%% execute shell
%% $ curl -sX POST localhost:8080/shell/{shell.dsl} \
%%        -H 'Content-Type: application/json' -d '{"arg":1}'
shell(post, Request) :-
    ( http_read_json_dict(Request, Params, []); Params = _{} ),
    http_log('~w~n', [params(Params)]),
    ( memberchk(path_info(File), Request),
      option(namespace(NS), Request)
      -> atomics_to_string([NS, "/", File], NS_File),
         cdb_api:doc_read(faasshell, NS_File, Code, Dict),
         http_log('~w~n', [doc_read(NS_File, Code)]),
         ( Code = 200
           -> term_string(Dsl, Dict.dsl),
              % http_log('~w~n', [dsl(Dsl)]),
              ( Dsl = asl(_)
                -> option(api_key(ID-PW), Request),
                   wsk_api_utils:openwhisk(Defaults),
                   merge_options([api_key(ID-PW)], Defaults, Options),
                   asl_run:start(Dsl, Options, Params, O),
                   Output = _{output:O}
                ;  Output = _{output:ng, error: Dict.dsl}
              )
           ;  Output = _{output:ng}.put(Dict)
         )
      ; Output = _{output:ng, error:"shell name missing!"}
    ),
    reply_json_dict(Output).

%% delete shell
%% $ curl -sX DELETE localhost:8080/shell/{shell.dsl}
shell(delete, Request) :-
    ( memberchk(path_info(File), Request),
      option(namespace(NS), Request)
      -> atomics_to_string([NS, "/", File], NS_File),
         cdb_api:doc_delete(faasshell, NS_File, Code, Res),
         http_log('~w~n', [doc_delete(NS_File, Code, Res)]),
         ( Code = 200
           -> Output = _{output:ok}
           ;  Output = _{output:ng}.put(Res)
         )
      ; Output = _{output:ng, error: "shell name missing!"}
    ),
    reply_json_dict(Output).

/*******************************
 *   PLUGIN FOR HTTP_DISPATCH   *
 *******************************/
:- multifile
http:authenticate/3.

:- use_module(library(debug)).

%% $ swipl -q -l src/asl_svc.pl -g asl_svc:debug_auth -g main -t halt
debug_auth :- debug(http_authenticate > user_error).

%%
:- dynamic
       cached_auth/4. % cached_auth(User, Password, Id, Time)

http:authenticate(openwhisk, Request, [api_key(User-Password), namespace(Id)]) :-
    memberchk(authorization(Text), Request),
    debug(http_authenticate, 'Authorization: ~w~n', [Text]),
    http_authorization_data(Text, basic(User, PasswordCode)),
    atom_codes(Password, PasswordCode),
    debug(http_authenticate, 'User: ~w, Password: ~s~n', [User, Password]),
    ( cached_auth(User, Password, Id, Time),
      get_time(Now),
      Now-Time =< 60
      -> debug(http_authenticate, 'Hit Cache: ~w, ~w~n', [User, Time]),
         http_log('Subject(cache): ~w, ~w~n', [User, Time]),
         true
      ;  ( retract(cached_auth(User, Password, Id, Time))
           -> debug(http_authenticate, 'retracted cache: ~w, ~w~n', [User, Time])
           ;  debug(http_authenticate, 'cache not exist: ~w, ~w~n', [User, Time]),
              true
         ),
         format(string(Query), '?startkey=[~w,~w]',[User, Password]),
         uri_encoded(query_value, Query, EncodedQuery),
         cdb_api:view_read(whisk_local_subjects, subjects, identities,
                           EncodedQuery, Code, Dict),
         ( Code = 200
           -> [Row0 |_] = Dict.rows,
              debug(http_authenticate, 'Subject: ~w~n Row0: ~w~n', [Dict, Row0]),
              atom_string(User, UserStr),
              atom_string(Password, PasswordStr),
              ( _{id:IdStr, key:[UserStr,PasswordStr], value:_} :< Row0
                -> atom_string(Id, IdStr),
                   get_time(Updated),
                   assertz(cached_auth(User, Password, Id, Updated)),
                   http_log('Subject(refresh): ~w, ~w-~w~n', [User, Time, Updated])
                ;  http_log('Auth failed: ~w, ~w~n', [User, Password]),
                   throw(http_reply(authorise(basic, openwhisk(User))))
              )
          ; http_log('Subject DB read failed: ~w~n', [Dict]),
            throw(http_reply(resource_error(format('DB status: ~wn', [Code]))))
         )
    ).
