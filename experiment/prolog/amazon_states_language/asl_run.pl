%% -*- mode: prolog; coding: utf-8; -*-
%%
%% $Id$
%%
%% Amazon State Language (ALS) DSL Interpreter
%%

:- module(asl_run,
          [ start/3
         ]).

:- use_module(wsk_api_utils).
:- use_module(wsk_api_actions).

:- use_module(library(http/json)).

mydebug(F, M) :- format("mydbug:~w: ~p~n", [F, M]).

start(File, I, O) :-
    open(File, read, S),
    call_cleanup(
            read_term(S, Term, []),
            close(S)),
    mydebug(start(in), (Term, I, O)),
    wsk_api_utils:openwhisk(Options),
    reduce(Term, I, O, _{openwhisk: Options}),
    mydebug(start(out), (I, O)).
              

reduce(asl(DSL), I, O, E) :- 
    !,
    mydebug(reduce(asl(in)), (I, O)),
    reduce(DSL, I, O, E),
    mydebug(reduce(asl(out)), (I, O)).
reduce([], O, O, _E) :-
    !,
    mydebug(reduce(done), O).
reduce([A|B], I, O, E) :-
    !,
    mydebug(reduce(bin(in)), (I, P)),
    reduce(A, I, P, E),
    reduce(B, P, O, E),
    mydebug(reduce(bin(out)), (P, O)).
reduce(A, I, O, E) :-
    mydebug(reduce(op(in)), (I, O)),
    A =.. L, append(L, [I, O, E], L1),
    Q =.. L1, Q,
    mydebug(reduce(op(out)), (I, O)).

%% pass state
pass(State, Options, I, O, _E) :- 
    mydebug(pass(in), (State, Options, I, O)),
    atom_json_dict(Options, J, []),
    string_concat("$.", KeyStr, J.'ResultPath'),
    atom_string(Key, KeyStr),
    Dict = I.put(Key, J.'Result'),
    wsk_api_utils:term_json_dict(O, Dict),
    mydebug(pass(out), (I, O)).

%% task state
task(State, Action, I, O, E) :-
    mydebug(task(in), (State, Action, I, O)),
    wsk_api_actions:invoke(hello, E.openwhisk, I, O),
    mydebug(pass(out), (I, O)).

%% choices state
choices(State, [Case|Cases], I, O) :-
    mydebug(choices(in), (State, Case, I, O)),
    reduce(Cases, I, O),
    mydebug(choices(out), (State, Case, I, O)).


%%
%% Unit Tests
%%
:- begin_tests(pass).

test(hello, O = json([message='Hello World!', name=wsk])) :-
    start('hello.dsl', _{name:"wsk"}, O).

:- end_tests(pass).

:- begin_tests(task).

test(hello, O = json([payload='Hello, wsk!'])) :-
    start('hello_task.dsl', _{name:"wsk"}, O).

:- end_tests(task).
