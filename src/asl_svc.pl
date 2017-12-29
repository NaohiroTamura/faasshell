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
%% $ curl -sLX GET localhost:8080/statemachine/{statemachine}
%% $ curl -sLX GET localhost:8080/statemachine/
statemachine(get, Request) :-
    ( memberchk(path_info(File), Request)
      -> cdb_api:doc_read(faasshell, File, Code1, Dict1),
         ( Code1 = 200
           -> Output = _{output:ok}.put(Dict1)
           ;  Output = _{output:ng}.put(Dict1)
         ),
         http_log('~w~n', [doc_read(Code1)])
      ;  cdb_api:view_read(faasshell, faas, statemachine, Code2, Dict2),
         ( Code2 = 200
           -> Output = _{output:ok}.put(Dict2)
           ;  Output = _{output:ng}.put(Dict2)
         ),
         http_log('~w~n', [view_read(Code2)])
    ),
    reply_json_dict(Output).

%% create state machine
%% $ curl -sX PUT localhost:8080/statemachine/{statemachine}?overwrite=true
%%        -H 'Content-Type: application/json' -d @asl.json
statemachine(put, Request) :-
    http_read_json_dict(Request, Params, []),
    http_log('~w~n', [params(Params)]),
    ( memberchk(path_info(File), Request)
      -> Dict = Params.put(name, File)
      ;  Dict = Params
    ),
    asl_gen:gen_dsl(Dict.asl, Dsl),
    http_log('~w~n', [dsl(Dsl)]),
    term_string(Dsl, DslStr),
    ( Dsl = asl(_)
      -> http_parameters(Request, [overwrite(Overwrite, [default(false)])]),
         http_log('~w~n', [overwrite(Overwrite)]),
         ( Overwrite = false
           -> cdb_api:doc_create(faasshell, Dict.name, Dict, Code, Res),
              http_log('~w~n', [doc_create(Code, Res)])
           ;  cdb_api:doc_update(faasshell, Dict.name, Dict, Code, Res),
              http_log('~w~n', [doc_update(Code, Res)])
         ),
         ( Code = 201
           -> Output = Dict.put(_{output:ok, dsl: DslStr})
           ;  Output = Dict.put(_{output:ng}).put(Res)
         )
      ;  Output = Dict.put(_{output:ng, error: "syntax error!", dsl: DslStr})
    ),
    reply_json_dict(Output).

%% execute state machine
%% $ curl -sX POST localhost:8080/statemachine/{statemachine} \
%%        -H 'Content-Type: application/json' -d '{"input":{"arg":1}}'
statemachine(post, Request) :-
    ( http_read_json_dict(Request, Params, []); Params = _{} ),
    http_log('~w~n', [params(Params)]),
    ( memberchk(path_info(File), Request)
      -> cdb_api:doc_read(faasshell, File, Code, Dict),
         http_log('~w~n', [doc_read(File, Code)]),
         ( Code = 200
           -> asl_gen:gen_dsl(Dict.asl, Dsl),
              % http_log('~w~n', [dsl(Dsl)]),
              term_string(Dsl, DslStr),
              ( Dsl = asl(_)
                -> Input = Dict.put(Params),
                   asl_run:start(Dsl, Input.input, O),
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
    ( memberchk(path_info(File), Request)
      -> cdb_api:doc_delete(faasshell, File, Code, Res),
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
    memberchk(path_info(File), Request)
    -> cdb_api:doc_read(faasshell, File, Code, Dict),
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
                [methods([get, put, post, delete]), prefix]).

shell(Request) :-
    http_log('~w~n', [request(Request)]),
    memberchk(method(Method), Request),
    shell(Method, Request).

%% get shell information
%% $ curl -sLX GET localhost:8080/shell/{shell.dsl}
%% $ curl -sLX GET localhost:8080/shell
shell(get, Request) :-
    ( memberchk(path_info(File), Request)
      -> cdb_api:doc_read(faasshell, File, Code1, Dict1),
         ( Code1 = 200
           -> Output = _{output:ok, dsl: Dict1.dsl}
           ;  Output = _{output:ng}.put(Dict1)
         ),
         http_log('~w~n', [doc_read(Code1)])
      ;  cdb_api:view_read(faasshell, faas, shell, Code2, Dict2),
         ( Code2 = 200
           -> Output = _{output:ok}.put(Dict2)
           ;  Output = _{output:ng}.put(Dict2)
         ),
         http_log('~w~n', [view_read(Code2)])
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
      -> ( memberchk(path_info(File), Request)
           -> http_parameters(Request, [overwrite(Overwrite, [default(false)])]),
              http_log('~w~n', [overwrite(Overwrite)]),
              ( Overwrite = false
                -> cdb_api:doc_create(faasshell, File, _{dsl: DslStr}, Code, Res),
                   http_log('~w~n', [doc_create(Code, Res)])
                ;  cdb_api:doc_update(faasshell, File, _{dsl: DslStr}, Code, Res),
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
    ( memberchk(path_info(File), Request)
      -> cdb_api:doc_read(faasshell, File, Code, Dict),
         http_log('~w~n', [doc_read(File, Code)]),
         ( Code = 200
           -> term_string(Dsl, Dict.dsl),
              % http_log('~w~n', [dsl(Dsl)]),
              ( Dsl = asl(_)
                -> asl_run:start(Dsl, Params, O),
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
    ( memberchk(path_info(File), Request)
      -> cdb_api:doc_delete(faasshell, File, Code, Res),
         http_log('~w~n', [doc_delete(Code, Res)]),
         ( Code = 200
           -> Output = _{output:ok}
           ;  Output = _{output:ng}.put(Res)
         )
      ; Output = _{output:ng, error: "shell name missing!"}
    ),
    reply_json_dict(Output).
