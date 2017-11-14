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
%%
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
%%

%% pass state
pass(State, Optional, I, O, _E) :- 
    mydebug(pass(in), (State, Optional, I, O)),
    option(result_path(ResultPath), Optional),
    option(result(Result), Optional),
    dollarvar_key(ResultPath, ResultPathKey),
    Dict = I.put(ResultPathKey, Result),
    wsk_api_utils:term_json_dict(O, Dict),
    mydebug(pass(out), (State, I, O)).

%% task state
task(State, Action, Optional, I, O, E) :-
    mydebug(task(in), (State, Action, Optional, I, O)),
    option(retry(R), Optional, []),
    option(fallback(F), Optional, []),
    retry(Action, R, I, O, E),
    mydebug(task(out), (I, O)).

retry(Action, Retry, I, O, E) :-
    mydebug(task(retry(in)), (I, O)),
    catch(
            wsk_api_actions:invoke(Action, E.openwhisk, I, O),
            Err,
            O = Err), %_{type:"Private",value:25}),  % print_message(error,Err)),
    mydebug(task(retry(out)), (I, O)).

%% choices state
choices(State, [], Optional, I, O, E) :-
    ( option(default(States), Optional)
      -> mydebug(choices(default(in)), (State, I, O)),
         reduce(States, I, O, E),
         mydebug(choices(default(out)), (State, I, O))
      ;  mydebug(choices(done(in)), (State, I, O)),
         O = I,
         mydebug(choices(done(out)), (State, I, O))
    ).

choices(State, [case(Cond,States)|Cases], Optional, I, O, E) :-
    mydebug(choices(in), (State, case(Cond), I, O)),
    reduce(Cond, I, M, E),
    (
        M == true
        -> mydebug(choices(M), (State, case(Cond), I, O)),
           case(States, I, O, E)
        ;  mydebug(choices(M), (State, case(Cond), I, O)),
           choices(State, Cases, Optional, I, O, E)
    ),
    mydebug(choices(out), (State, case(Cond), I, O)).

case([], O, O, _E) :-
    mydebug(case(true(done)), O).
case([State|States], I, O, E) :-
    mydebug(case(true(in)), (State, I, O)),
    reduce(State, I, M, E),
    case(States, M, O, E),
    mydebug(case(true(out)), (State, I, O)).

'Not'(Cond, I, O, E) :-
    mydebug('Not'(in), (Cond, I, O)),
    reduce(Cond, I, M, E),
    not(M, O),
    mydebug('Not'(out), (Cond, I, O)).

'And'([], _I, true, _E) :-
    mydebug('And'(done), true).
'And'([Cond|Conds], I, O, E) :-
    mydebug('And'(in), ([Cond|Conds], I, O)),
    reduce(Cond, I, M1, E),
    'And'(Conds, I, M2, E),
    and(M1, M2, O),
    mydebug('And'(out), ([Cond|Conds], I, O)).

'Or'([], _I, false, _E) :-
    mydebug('Or'(done), false).
'Or'([Cond|Conds], I, O, E) :-
    mydebug('Or'(in), ([Cond|Conds], I, O)),
    reduce(Cond, I, M1, E),
    'Or'(Conds, I, M2, E),
    or(M1, M2, O),
    mydebug('Or'(out), ([Cond|Conds], I, O)).

'NumericEquals'(DollarVar, Value, I, O, _E) :-
    mydebug('NumericEquals'(in), (DollarVar, Value, I, O)),
    dollarvar_key(DollarVar, Key),
    ( I.Key == Value ->  O = true; O = false ),
    mydebug('NumericEquals'(out), (DollarVar, Value, I, O)).

'NumericGreaterThanEquals'(DollarVar, Value, I, O, _E) :-
    mydebug('NumericGreaterThanEquals'(in), (DollarVar, Value, I, O)),
    dollarvar_key(DollarVar, Key),
    ( I.Key >= Value ->  O = true; O = false ),
    mydebug('NumericGreaterThanEquals'(out), (DollarVar, Value, I, O)).

'NumericLessThan'(DollarVar, Value, I, O, _E) :-
    mydebug('NumericLessThan'(in), (DollarVar, Value, I, O)),
    dollarvar_key(DollarVar, Key),
    ( I.Key < Value ->  O = true; O = false ),
    mydebug('NumericLessThan'(out), (DollarVar, Value, I, O)).

'StringEquals'(DollarVar, Value, I, O, _E) :-
    mydebug('StringEquals'(in), (DollarVar, Value, I, O)),
    dollarvar_key(DollarVar, Key),
    ( I.Key == Value ->  O = true; O = false ),
    mydebug('StringEquals'(out), (DollarVar, Value, I, O)).

%%
%% fail state
fail(State, Optional, I, O, _E) :-
    mydebug(fail(in), (State, Optional, I, O)),
    (option(cause(Cause), Optional) -> O1 = I.put(cause, Cause); O1 = I),
    (option(error(Error), Optional) -> O2 = O1.put(error, Error); O2 = O1),
    O = O2,
    mydebug(fail(out), (State, I, O)).

%%
%% misc.
%%
dollarvar_key(DollarVar, Key) :-
    string_concat("$.", KeyStr, DollarVar),
    atom_string(Key, KeyStr).

not(true, false).
not(false, true).

and(true, true, true).
and(true, false, false).
and(false, true, false).
and(false, false, false).

or(true, true, true).
or(true, false, true).
or(false, true, true).
or(false, false, false).

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
