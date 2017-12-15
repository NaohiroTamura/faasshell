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
      -> load_json(File, Output)
      ;  Output = _{output:ng, error:"statemashine missing!"}
    ),
    reply_json_dict(Output).

%% create state machine
%% $ curl -X PUT -H 'Content-Type: application/json' \
%%        -H 'Accept: application/json' -d @asl.json \
%%         localhost:8080/statemachine/{overwrite statemachine name}
statemachine(put, Request) :-
    http_read_json_dict(Request, Params, []),
    http_log('~w~n', [params(Params)]),
    ( memberchk(path_info(Name), Request)
      -> Dict = Params.put(name, Name)
      ;  Dict = Params
    ),
    % TODO: save ASL and DSL into DB
    save_json(Dict, File),
    http_log('~w~n', [save_json(File)]),
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
    %% TODO: read Dsl from DB
    http_read_json_dict(Request, Params, []),
    http_log('~w~n', [params(Params)]),
    ( memberchk(path_info(File), Request)
      -> load_json(File, Dict),
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
    -> delete_rc(File),
       reply_json_dict(_{output: ok})
    ;  reply_json_dict(_{output:ng, error:"statemashine missing!"}).

%% create graph of state machine
%% $ curl -X PATTCH localhost:8080/statemachine/{statemachine}
statemachine(patch, Request) :-
    memberchk(path_info(File), Request)
    -> load_json(File, Dict),
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
      -> load_term(File, Dsl),
         term_string(Dsl, DslStr),
         Output = _{output:ok, dsl:DslStr}
      ;  Output = _{output:ng, error:"shell name missing!"}
    ),
    reply_json_dict(Output).

%% create shell
%% $ curl -X PUT -H 'Content-Type: text/plain' \
%%        -H 'Accept: application/json' -d @shell.dsl \
%%         localhost:8080/shell/{shell.dsl}
shell(put, Request) :-
    ( memberchk(path_info(Name), Request)
      -> http_read_data(Request, DslStr, [application/x-prolog]),
         term_string(Dsl, DslStr),
         http_log('~w~n', [put(Dsl)]),
         ( Dsl = asl(_)
           -> % TODO: save ASL and DSL into DB
               save_term(Name, Dsl, File),
               http_log('~w~n', [save_term(File)]),
               Output = _{output:ok, dsl:DslStr}
           ;   Output = _{output:ng, error:DslStr}
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
    ( memberchk(path_info(Name), Request)
      -> load_term(Name, Dsl),
         term_string(Dsl, DslStr),
         http_log('~w~n', [post(params(Params), dsl(Dsl))]),
         ( Dsl = asl(_)
           -> asl_run:start(Dsl, Params, O),
              Output = _{output:O}
           ;  Output = _{output:ng, error:DslStr}
         )
      ; Output = _{output:ng, error:"shell name missing!"}
    ),
    reply_json_dict(Output).

%% delete shell
%% $ curl -X DELETE localhost:8080/shell/{shell.dsl}
shell(delete, Request) :-
    memberchk(path_info(File), Request)
    -> delete_rc(File),
       reply_json_dict(_{output:ok})
    ;  reply_json_dict(_{output:ng, error:"shell name missing!"}).

%%
%% TODO: implement session management in managed DB
%%
save_term(Name, Term, File) :-
    string_concat("/logs/", Name, File),
    open(File, write, S),
    call_cleanup(
            %%write_term(S, Term, []),
            format(S, '~p.~n', [Term]),
            close(S)).

load_term(Name, Term) :-
    string_concat("/logs/", Name, File),
    open(File, read, S),
    call_cleanup(
            read_term(S, Term, []),
            close(S)).

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

delete_rc(Name) :-
    string_concat("/logs/", Name, File),
    delete_file(File).
