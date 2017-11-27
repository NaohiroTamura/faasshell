%% #!/usr/bin/swipl -q
%% -*- mode: prolog; coding: utf-8; -*-
%%
%% $Id$
%%

:- use_module(asl_svc, [main/0]).

:- use_module(asl_gen, [gen_dsl/2]).
:- use_module(asl_run, [start/3]).

%% http server
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_log)).
%:- use_module(library(http/http_error)). % should be removed in puroduction

%% start
%% :- initialization(main).

%%
%% main
%%
%% start:
%%   ?- asl_svc:main.
%% stop the server:
%%   ?- http_stop_server(8080,[]).
%%
main :-
    getenv('SVC_PORT', Port) -> server(Port); server(8080).

server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    thread_get_message(stop).

%% signal handler
:- on_signal(hup, _, hup).

hup(_Signal) :-
    thread_send_message(main, stop),
    halt(0).

%% $ curl -i localhost:8080/list/
%% $ curl -i localhost:8080/list/{actionName}
:- http_handler('/list/', list, [methods([get]), prefix]).

list(Request) :-
    % http_log('~w', [request(Request)]),
    wsk_api_utils:openwhisk(Options),
    ( memberchk(path_info(Action), Request)
      -> wsk_api_actions:list(Action, Options, Reply)
      ;  wsk_api_actions:list(none, Options, Reply)
    ),
    reply_json_dict(Reply).

%% $ curl -i -X POST -H 'Content-Type: application/json' \
%%           -H 'Accept: application/json' -d @asl.json \
%%           localhost:8080/validate
:- http_handler('/validate', validate, [methods([post])]).

validate(Request) :-
    http_read_json_dict(Request, Params, []), 
    % http_log('~w~n', [params(Params)]),
    asl_gen:gen_dsl(Params.asl, Dsl),
    % http_log('~w~n', [dsl(Dsl)]),
    term_string(Dsl, DslStr),
    ( Dsl = asl(_)
      -> Output = Params.put(_{output:ok, dsl:DslStr})
      ;  Output = Params.put(_{output:ng, error:DslStr})
    ),
    reply_json_dict(Output).

%% $ curl -i -X POST -H 'Content-Type: application/json' \
%%           -H 'Accept: application/json' -d @asl.json \
%%           localhost:8080/run
:- http_handler('/run', run, [methods([post])]).

run(Request) :-
    http_read_json_dict(Request, Params, []), 
    % http_log('~w~n', [params(Params)]),
    asl_gen:gen_dsl(Params.asl, Dsl),
    % http_log('~w~n', [dsl(Dsl)]),
    term_string(Dsl, DslStr),
    ( Dsl = asl(_)
      -> asl_run:start(Dsl, Params.input, O),
         Output = Params.put(_{output:O})
      ;  Output = Params.put(_{output:ng, error:DslStr})
    ),
    reply_json_dict(Output).

%% $ curl -X POST -H 'Content-Type: application/json' \
%%        -H 'Accept: application/json' -d @asl.json \
%%        localhost:8080/graph
:- http_handler('/graph', graph, [methods([post])]).

graph(Request) :-
    http_read_json_dict(Request, Params, []), 
    format('Content-type: text/plain~n~n'),
    % http_log('~w~n', [params(Params)]),
    asl_gen:gen_dot(Params.asl).
