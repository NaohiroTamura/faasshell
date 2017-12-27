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
:- use_module(asl_run, [start/3]).
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
    getenv('SVC_PORT', Port) -> server(Port); server(8080).

server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    thread_get_message(stop).

%% signal handler
:- on_signal(hup, _, hup).

hup(_Signal) :-
    thread_send_message(main, stop),
    halt(0).

%% $ curl -i localhost:8080/faas
%% $ curl -i localhost:8080/faas/{namespace}
%% $ curl -i localhost:8080/faas/{namespace}/{actionName}
:- http_handler('/faas', faas, [methods([get]), prefix]).

faas(Request) :-
    % http_log('~w~n', [request(Request)]),
    wsk_api_utils:openwhisk(Options),
    ( memberchk(path_info(Action), Request)
      -> wsk_api_actions:list(Action, Options, Reply)
      ;  wsk_api_actions:list(none, Options, Reply)
    ),
    reply_json_dict(Reply).

%%    GET: get statemachine information
%%    PUT: create statemachine
%%   POST: execute statemachine
%% DELETE: delete statemachine
%% PATCH : create graph of statemachine
:- http_handler('/statemachine/', statemachine,
                [methods([get, put, post, delete, patch]), prefix]).

statemachine(Request) :-
    http_log('~w~n', [request(Request)]),
    memberchk(method(Method), Request),
    statemachine(Method, Request).

%% get state machine information
%% $ curl localhost:8080/statemachine/{statemachine}
statemachine(get, Request) :-
    ( memberchk(path_info(File), Request)
      -> cdb_api:doc_read(faasshell, File, Code, Output),
         http_log('~w~n', [doc_read(Code)])
      ;  Output = _{output:ng, error:"statemashine missing!"}
    ),
    reply_json_dict(Output).

%% create state machine
%% $ curl -X PUT -H 'Content-Type: application/json' \
%%        -H 'Accept: application/json' -d @asl.json \
%%         localhost:8080/statemachine/{statemachine}
statemachine(put, Request) :-
    http_read_json_dict(Request, Params, []),
    http_log('~w~n', [params(Params)]),
    ( memberchk(path_info(File), Request)
      -> Dict = Params.put(name, File)
      ;  Dict = Params
    ),
    http_parameters(Request, [overwrite(Overwrite, [default(false)])]),
    http_log('~w~n', [overwrite(Overwrite)]),
    ( Overwrite = false
      -> cdb_api:doc_create(faasshell, Dict.name, Dict, Code, Res),
         % TODO: error check
         http_log('~w~n', [doc_create(Code, Res)])
      ;  cdb_api:doc_read(faasshell, Dict.name, Code1, R1),
         % TODO: error check
         % http_log('~w~n', [doc_read(Dict.name, Code1, R1)]),
         cdb_api:doc_update(faasshell, Dict.name, R1.'_rev', Dict, Code2, Res),
         http_log('~w~n', [doc_update(Code1, Code2, Res)])
    ),
    asl_gen:gen_dsl(Dict.asl, Dsl),
    http_log('~w~n', [dsl(Dsl)]),
    term_string(Dsl, DslStr),
    ( Dsl = asl(_)
      -> Output = Dict.put(_{output:ok, dsl:DslStr})
      ;  Output = Dict.put(_{output:ng, error:DslStr})
    ),
    reply_json_dict(Output).

%% execute state machine
%% $ curl -X POST -H 'Content-Type: application/json' \
%%        -H 'Accept: application/json' -d '{"input":{"arg":"overwrite"}' \
%%        localhost:8080/statemachine/{statemachine}
statemachine(post, Request) :-
    ( http_read_json_dict(Request, Params, []); Params = _{}),
    http_log('~w~n', [params(Params)]),
    ( memberchk(path_info(File), Request)
      -> cdb_api:doc_read(faasshell, File, Code, Dict),
         http_log('~w~n', [doc_read(Code)]),
         asl_gen:gen_dsl(Dict.asl, Dsl),
         % http_log('~w~n', [dsl(Dsl)]),
         term_string(Dsl, DslStr),
         ( Dsl = asl(_)
           -> Input = Dict.put(Params),
              asl_run:start(Dsl, Input.input, O),
              Output = Dict.put(_{output:O})
           ;  Output = Dict.put(_{output:ng, error:DslStr})
         )
      ;  Output = _{output:ng, error:"statemashine missing!"}
    ),
    reply_json_dict(Output).

%% delete state machine
%% $ curl -X DELETE localhost:8080/statemachine/{statemachine}
statemachine(delete, Request) :-
    memberchk(path_info(File), Request)
    -> cdb_api:doc_read(faasshell, File, Code1, R1),
       cdb_api:doc_delete(faasshell, File, R1.'_rev', Code2, Res),
       http_log('~w~n', [doc_delete(Code1, Code2, Res)]),
       reply_json_dict(_{output: ok})
    ;  reply_json_dict(_{output:ng, error:"statemashine missing!"}).

%% create graph of state machine
%% $ curl -X PATTCH localhost:8080/statemachine/{statemachine}
statemachine(patch, Request) :-
    memberchk(path_info(File), Request)
    -> cdb_api:doc_read(faasshell, File, Code, Dict),
       http_log('~w~n', [doc_read(Code)]),
       format('Content-type: text/plain~n~n'),
       asl_gen:gen_dot(Dict.asl)
    ;  reply_json_dict(_{output:ng, error:"statemashine missing!"}).

%%    GET: get shell.dsl information
%%    PUT: create shell.dsl
%%   POST: execute shell.dsl
%% DELETE: delete shell.dsl
:- http_handler('/shell/', shell,
                [methods([get, put, post, delete]), prefix]).

shell(Request) :-
    http_log('~w~n', [request(Request)]),
    memberchk(method(Method), Request),
    shell(Method, Request).

%% get shell information
%% $ curl localhost:8080/shell/{shell.dsl}
shell(get, Request) :-
    ( memberchk(path_info(File), Request)
      -> cdb_api:doc_read(faasshell, File, Code, Dict),
         http_log('~w~n', [doc_read(File, Code)]),
         Output = _{output:ok, dsl: Dict.dsl}
      ;  Output = _{output:ng, error:"shell name missing!"}
    ),
    reply_json_dict(Output).

%% create shell
%% $ curl -X PUT -H 'Content-Type: text/plain' \
%%        -H 'Accept: application/json' -d @shell.dsl \
%%         localhost:8080/shell/{shell.dsl}
shell(put, Request) :-
    ( memberchk(path_info(File), Request)
      -> http_read_data(Request, DslStr, [application/x-prolog]),
         term_string(Dsl, DslStr),
         http_log('~w~n', [put(Dsl)]),
         ( Dsl = asl(_)
           -> http_parameters(Request, [overwrite(Overwrite, [default(false)])]),
              http_log('~w~n', [overwrite(Overwrite)]),
              ( Overwrite = false
                -> cdb_api:doc_create(faasshell, File, _{dsl: DslStr}, Code, Res),
                   http_log('~w~n', [doc_create(Code, Res)])
                ; cdb_api:doc_read(faasshell, File, Code1, R1),
                  % http_log('~w~n', [doc_read(File, Code1, R1)]),
                  cdb_api:doc_update(faasshell, File, R1.'_rev',
                                               _{dsl: DslStr}, Code2, Res),
                  http_log('~w~n', [doc_update(Code1, Code2, Res)])
              ),
              Output = _{output:ok, dsl: DslStr}
           ;  Output = _{output:ng, error: DslStr}
         )
      ; Output = _{output:ng, error:"shell name missing!"}
    ),
    reply_json_dict(Output).

%% execute shell
%% $ curl -X POST -H 'Content-Type: application/json' \
%%        -H 'Accept: application/json' -d '{"arg":1}' \
%%         localhost:8080/shell/{shell.dsl}
shell(post, Request) :-
    http_read_json_dict(Request, Params, []),
    ( memberchk(path_info(File), Request)
      -> cdb_api:doc_read(faasshell, File, Code, Dict),
         http_log('~w~n', [doc_read(File, Code)]),
         term_string(Dsl, Dict.dsl),
         http_log('~w~n', [post(params(Params), dsl(Dsl))]),
         ( Dsl = asl(_)
           -> asl_run:start(Dsl, Params, O),
              Output = _{output:O}
           ;  Output = _{output:ng, error: Dict.dsl}
         )
      ; Output = _{output:ng, error:"shell name missing!"}
    ),
    reply_json_dict(Output).

%% delete shell
%% $ curl -X DELETE localhost:8080/shell/{shell.dsl}
shell(delete, Request) :-
    memberchk(path_info(File), Request)
    -> cdb_api:doc_read(faasshell, File, Code1, R1),
       cdb_api:doc_delete(faasshell, File, R1.'_rev', Code2, Res),
       http_log('~w~n', [doc_delete(Code1, Code2, Res)]),
       reply_json_dict(_{output:ok})
    ;  reply_json_dict(_{output:ng, error:"shell name missing!"}).
