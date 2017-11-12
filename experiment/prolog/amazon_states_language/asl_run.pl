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

mydebug(F, M) :- format("mydbug: ~p~t~24| : ~p~n", [F, M]).

start(File, I, O) :-
    open(File, read, S),
    call_cleanup(
            read_term(S, Term, []),
            close(S)),
    mydebug(start(in), (Term, I, O)),
    wsk_api_utils:openwhisk(Options),
    reduce(Term, I, O, _{openwhisk: Options}),
    mydebug(start(out), (I, O)).
              
%%
%% Meta Circular Interpreter
%% reduce(+DSL, +Input, -Output, +Environment)
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
    mydebug(reduce(bin(in)), (I, O)),
    reduce(A, I, M, E), % M stands for Middle state
    reduce(B, M, O, E),
    mydebug(reduce(bin(out)), (M, O)).
reduce(A, I, O, E) :-
    mydebug(reduce(op(in)), (I, O)),
    A =.. L, append(L, [I, O, E], L1),
    Q =.. L1, Q,
    mydebug(reduce(op(out)), (I, O)).

%% pass state
pass(State, Options, I, O, _E) :- 
    mydebug(pass(in), (State, Options, I, O)),
    atom_json_dict(Options, J, []),
    dollarvar_key(J.'ResultPath', Key),
    Dict = I.put(Key, J.'Result'),
    wsk_api_utils:term_json_dict(O, Dict),
    mydebug(pass(out), (State, I, O)).

%% task state
task(State, Action, I, O, E) :-
    mydebug(task(in), (State, Action, I, O)),
    catch(
            wsk_api_actions:invoke(hello, E.openwhisk, I, O),
            Err,
            O = Err), % print_message(error,Err)),
    mydebug(task(out), (I, O)).

%% choices state
choices(State, [], O, O, _E) :-
    mydebug(choices(done), (State, O)).
choices(State, [default(States)], I, O, E) :-
    mydebug(choices(default), (State, I, O)),
    default(States, I, O, E).
choices(State, [case(Cond,States)|Cases], I, O, E) :-
    mydebug(choices(in), (State, case(Cond), I, O)),
    reduce(Cond, I, M, E),
    (
        M == true
        -> mydebug(choices(M), (State, case(Cond), I, O)),
           case(States, I, O, E)
        ;  mydebug(choices(M), (State, Case, I, O)),
           choices(State, Cases, I, O, E)
    ),
    mydebug(choices(out), (State, Case, I, O)).

case([], O, O, _E) :-
    mydebug(case(true(done)), O).
case([State|States], I, O, E) :-
    mydebug(case(true(in)), (State, I, O)),
    reduce(State, I, M, E),
    case(States, M, O, E),
    mydebug(case(true(out)), (State, I, O)).

default([], O, O, _E) :-
    mydebug(default(done), O).
default([State|States], I, O, E) :-
    mydebug(default(in), (State, I, O)),
    reduce(State, I, M, E),
    default(States, M, O, E),
    mydebug(default(out), (State, I, O)).

'NumericEquals'(DollarVar, Value, I, O, _E) :-
    mydebug('NumericEquals'(in), (DollarVar, Value, I, O)),
    dollarvar_key(DollarVar, Key),
    (
        I.Key == Value
        ->  O = true,
            mydebug('NumericEquals'(true), (DollarVar, Value, I, O))
        ;   O = false,
            mydebug('NumericEquals'(false), (DollarVar, Value, I, O))
    ),
    mydebug('NumericEquals'(out), (DollarVar, Value, I, O)).


%%
%% fail state
fail(State, Error, Reason, I, O, _E) :-
    mydebug(fail(in), (State, Error, Reason, I, O)),
    O = (Error, Reason),
    mydebug(fail(out), (State, I, O)).

%%
%% misc.
dollarvar_key(DollarVar, Key) :-
    string_concat("$.", KeyStr, DollarVar),
    atom_string(Key, KeyStr).

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

:- begin_tests(choice).

test(default, O = json([payload='Hello, wsk!'])) :-
    start('choice_state.dsl', _{foo:5}, O).

:- end_tests(choice).
