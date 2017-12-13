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

:- use_module(asl_svc, [main/0]).

:- use_module(asl_gen, [gen_dsl/2]).
:- use_module(asl_run, [start/3]).

%% http server
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_log)).
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
    ( Method = put
      -> http_read_json_dict(Request, Params, []),
         http_log('~w~n', [params(Params)]),
         statemachine(Method, Params, Reply),
         reply_json_dict(Reply)
      ;  statemachine(Method, Request)
    ).

%% get state machine information
%% $ curl localhost:8080/statemachine/{statemachine}
statemachine(get, Request) :-
    ( memberchk(path_info(File), Request)
      -> load_json(File, Output)
      ;  Output = _{output: 'TODO'}
    ),
    reply_json_dict(Output).

%% create state machine
%% $ curl -X PUT -H 'Content-Type: application/json' \
%%        -H 'Accept: application/json' -d @asl.json \
%%         localhost:8080/statemachine
statemachine(put, Params, Reply) :-
    http_log('~w~n', [put(Params.name)]),
    asl_gen:gen_dsl(Params.asl, Dsl),
    http_log('~w~n', [dsl(Dsl)]),
    term_string(Dsl, DslStr),
    ( Dsl = asl(_)
      -> % TODO: save ASL and DSL into DB
         save_json(Params, File),
         http_log('~w~n', [save_json(File)]),
         Reply = Params.put(_{output:ok, dsl:DslStr})
      ;  Reply = Params.put(_{output:ng, error:DslStr})
    ).

%% execute state machine
%% $ curl -X POST localhost:8080/statemachine/{statemachine}
statemachine(post, Request) :-
    %% TODO: read Dsl from DB
    ( memberchk(path_info(File), Request)
      -> load_json(File, Dict),
         asl_gen:gen_dsl(Dict.asl, Dsl),
         % http_log('~w~n', [dsl(Dsl)]),
         term_string(Dsl, DslStr),
         ( Dsl = asl(_)
           -> asl_run:start(Dsl, Dict.input, O),
              Output = Dict.put(_{output:O})
           ;  Output = Dict.put(_{output:ng, error:DslStr})
         )
      ;  Output = _{output: 'TODO'}
    ),
    reply_json_dict(Output).

%% execute state machine
%% $ curl -X DELETE localhost:8080/statemachine/{statemachine}
statemachine(delete, Request) :-
    memberchk(path_info(File), Request)
    -> delete_json(File),
       reply_json_dict(_{output: 'ok'})
    ;  reply_json_dict(_{output: 'TODO'}).

%% create graph of state machine
%% $ curl -X PATTCH localhost:8080/statemachine/{statemachine}
statemachine(patch, Request) :-
    memberchk(path_info(File), Request)
    -> load_json(File, Dict),
       format('Content-type: text/plain~n~n'),
       asl_gen:gen_dot(Dict.asl)
    ;  reply_json_dict(_{output: 'TODO'}).

%%
%% TODO: implement session management in managed DB
%%
save_json(Dict, File) :-
    string_concat("/logs/", Dict.name, File),
    open(File, write, S),
    call_cleanup(
            json_write_dict(S, Dict, []),
            close(S)).

load_json(Name, Dict) :-
    string_concat("/logs/", Name, File),
    open(File, read, S),
    call_cleanup(
            json_read_dict(S, Dict, []),
            close(S)).

delete_json(Name) :-
    string_concat("/logs/", Name, File),
    delete_file(File).
