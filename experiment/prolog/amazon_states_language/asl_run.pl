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
%% begin of iterpreter
%%
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
%% end of interpreter
%%

%%
%% begin of state
%%
%% pass state
pass(State, Optional, I, O, _E) :- 
    mydebug(pass(in), (State, Optional, I, O)),
    ( option(result(Result), Optional)
      -> process_output(I, Result, O, Optional)
      ;  O = I
    ),
    mydebug(pass(out), (State, I, O)).

%% task state
task(State, Action, Optional, I, O, E) :-
    mydebug(task(in), (State, Action, Optional, I, O)),
    process_input(I, I1, Optional),
    execute(Action, I1, M1, E),
    ( option(retry(R), Optional), is_dict(M1), get_dict(error, M1, Error1)
      -> retry(Action, R, Error1, M2, E.put(_{action_input:I1}))
      ;  M2 = M1
    ),
    ( option(fallback(F), Optional), is_dict(M2), get_dict(error, M2, Error2)
      -> fallback(State, F, Error2, M3, E)
      ;  M3 = M2
    ),
    process_output(I, M3, O, Optional),
    mydebug(task(out), (I, O)).

execute(Action, I, O, E) :-
    mydebug(task(execute(in)), (I, O)),
    catch(
            call_with_time_limit(1, sleep(2)),
            %% O = _{error: "xStates.TaskFailed"})), %% for test
            %% wsk_api_actions:invoke(Action, E.openwhisk, I, O),
            %% Err, print_message(error,Err)),
            Err, O = _{error: Err}), 
    mydebug(task(execute(out)), (I, O)).

retry(_Action, [], O, O, _E) :-
    mydebug(retry(done), O).
retry(Action, [case(Cond, Optional)|Cases], I, O, E) :-
    mydebug(task(retry(in)), (I, O)),
    reduce(Cond, I, M, E),
    (
        M == true
        -> mydebug(task(retry(true)), (case(Cond), I, O)),
           option(interval_seconds(IntervalSeconds), Optional, 1),
           option(max_attempts(MaxAttempts), Optional, 3),
           option(backoff_rate(BackoffRate), Optional, 2.0),
           option(current_attempt(CurrentAttempt), Optional, 0),
           mydebug(task(retry(sleep)), current_attempt(CurrentAttempt)),
           ( CurrentAttempt =:= 0
             -> sleep(IntervalSeconds),
                mydebug(task(retry(sleep)), first_interval(IntervalSeconds))
             ;  NewInterval is IntervalSeconds * BackoffRate^CurrentAttempt,
                sleep(NewInterval),
                mydebug(task(retry(sleep)), new_interval(NewInterval))
           ),
           ( CurrentAttempt < MaxAttempts
             -> NewAttempt is CurrentAttempt + 1,
                merge_options([current_attempt(NewAttempt)], Optional, NewOptional),
                execute(Action, E.action_input, M1, E),
                ( _{error: Err} :< M1 
                  -> mydebug(task(retry(again)), new_optional(NewOptional)),
                     retry(Action, [case(Cond, NewOptional)|Cases], Err, O, E)
                  ;  retry(Action, [], M1, O, E)
                )
             ; retry(Action, [], _{error:I}, O, E)
           )
        ;  mydebug(task(retry(false)), (case(Cond), I, O)),
           retry(Action, Cases, I, O, E)
    ),
    mydebug(task(retry(out)), (I, O)).

fallback(State, [], O, O, _E) :-
    mydebug(task(fallback(done)), (State, O)).
fallback(State, [case(Cond, States)|Cases], I, O, E) :-
    mydebug(task(fallback(in)), (State, case(Cond), I, O)),
    reduce(Cond, I, M, E),
    (
        M == true
        -> mydebug(task(fallback(true)), (State, case(Cond), I, O)),
           reduce(States, I, O, E)
        ;  mydebug(task(fallback(false)), (State, case(Cond), I, O)),
           fallback(Cases, I, O, E)
    ),
    mydebug(task(fallback(out)), (State, case(Cond), I, O)).

%% choices state
choices(State, [], Optional, I, O, E) :-
    option(default(States), Optional)
    -> mydebug(choices(default(in)), (State, I, O)),
       reduce(States, I, O, E),
       mydebug(choices(default(out)), (State, I, O))
    ;  mydebug(choices(done(in)), (State, I, O)),
       O = I,
       mydebug(choices(done(out)), (State, I, O)).

choices(State, [case(Cond, States)|Cases], Optional, I, O, E) :-
    mydebug(choices(in), (State, case(Cond), I, O)),
    reduce(Cond, I, M, E),
    (
        M == true
        -> mydebug(choices(true), (State, case(Cond), I, O)),
           reduce(States, I, O, E)
        ;  mydebug(choices(false), (State, case(Cond), I, O)),
           choices(State, Cases, Optional, I, O, E)
    ),
    mydebug(choices(out), (State, case(Cond), I, O)).

%% fail state
fail(State, Optional, I, O, _E) :-
    mydebug(fail(in), (State, Optional, I, O)),
    (option(cause(Cause), Optional) -> O1 = I.put(cause, Cause); O1 = I),
    (option(error(Error), Optional) -> O2 = O1.put(error, Error); O2 = O1),
    O = O2,
    mydebug(fail(out), (State, I, O)).
%% end of state
%%

%%
%% retry and fallback conditions
%%
'ErrorEquals'(["States.ALL"], I, true, _E) :- !,
    mydebug('ErrorEquals'("States.ALL"), (I, true)).
'ErrorEquals'(ErrorNames, I, O, _E) :-
    mydebug('ErrorEquals'(in), (ErrorNames, I, O)),
    (memberchk(I, ErrorNames) -> O = true; O = false),
    mydebug('ErrorEquals'(out), (ErrorNames, I, O)).

%%
%% choice conditions
%%
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

'BooleanEquals'(Variable, Value, I, O, _E) :-
    mydebug('BooleanEquals'(in), (Variable, Value, I, O)),
    ( I.Variable == Value ->  O = true; O = false ),
    mydebug('BooleanEquals'(out), (Variable, Value, I, O)).

'NumericEquals'(Variable, Value, I, O, _E) :-
    mydebug('NumericEquals'(in), (Variable, Value, I, O)),
    ( I.Variable =:= Value ->  O = true; O = false ),
    mydebug('NumericEquals'(out), (Variable, Value, I, O)).

'NumericGreaterThan'(Variable, Value, I, O, _E) :-
    mydebug('NumericGreaterThan'(in), (Variable, Value, I, O)),
    ( I.Variable > Value ->  O = true; O = false ),
    mydebug('NumericGreaterThan'(out), (Variable, Value, I, O)).

'NumericGreaterThanEquals'(Variable, Value, I, O, _E) :-
    mydebug('NumericGreaterThanEquals'(in), (Variable, Value, I, O)),
    ( I.Variable >= Value ->  O = true; O = false ),
    mydebug('NumericGreaterThanEquals'(out), (Variable, Value, I, O)).

'NumericLessThan'(Variable, Value, I, O, _E) :-
    mydebug('NumericLessThan'(in), (Variable, Value, I, O)),
    ( I.Variable < Value ->  O = true; O = false ),
    mydebug('NumericLessThan'(out), (Variable, Value, I, O)).

'NumericLessThanEquals'(Variable, Value, I, O, _E) :-
    mydebug('NumericLessThanEquals'(in), (Variable, Value, I, O)),
    ( I.Variable =< Value ->  O = true; O = false ),
    mydebug('NumericLessThanEquals'(out), (Variable, Value, I, O)).

'StringEquals'(Variable, Value, I, O, _E) :-
    mydebug('StringEquals'(in), (Variable, Value, I, O)),
    ( I.Variable == Value ->  O = true; O = false ),
    mydebug('StringEquals'(out), (Variable, Value, I, O)).

'StringGreaterThan'(Variable, Value, I, O, _E) :-
    mydebug('StringGreaterThan'(in), (Variable, Value, I, O)),
    ( I.Variable @> Value ->  O = true; O = false ),
    mydebug('StringGreaterThan'(out), (Variable, Value, I, O)).

'StringGreaterThanEquals'(Variable, Value, I, O, _E) :-
    mydebug('StringGreaterThanEquals'(in), (Variable, Value, I, O)),
    ( I.Variable @>= Value ->  O = true; O = false ),
    mydebug('StringGreaterThanEquals'(out), (Variable, Value, I, O)).

'StringLessThan'(Variable, Value, I, O, _E) :-
    mydebug('StringLessThan'(in), (Variable, Value, I, O)),
    ( I.Variable @< Value ->  O = true; O = false ),
    mydebug('StringLessThan'(out), (Variable, Value, I, O)).

'StringLessThanEquals'(Variable, Value, I, O, _E) :-
    mydebug('StringLessThanEquals'(in), (Variable, Value, I, O)),
    ( I.Variable @=< Value ->  O = true; O = false ),
    mydebug('StringLessThanEquals'(out), (Variable, Value, I, O)).

'TimestampEquals'(Variable, Value, I, O, _E) :-
    mydebug('TimestampEquals'(in), (Variable, Value, I, O)),
    parse_time(Variable, VariableStamp),
    parse_time(Value, ValueStamp),
    ( I.VariableStamp =:= ValueStamp ->  O = true; O = false ),
    mydebug('TimestampEquals'(out), (Variable, Value, I, O)).

'TimestampGreaterThan'(Variable, Value, I, O, _E) :-
    mydebug('TimestampGreaterThan'(in), (Variable, Value, I, O)),
    parse_time(Variable, VariableStamp),
    parse_time(Value, ValueStamp),
    ( I.VariableStamp > ValueStamp ->  O = true; O = false ),
    mydebug('TimestampGreaterThan'(out), (Variable, Value, I, O)).

'TimestampGreaterThanEquals'(Variable, Value, I, O, _E) :-
    mydebug('TimestampGreaterThanEquals'(in), (Variable, Value, I, O)),
    parse_time(Variable, VariableStamp),
    parse_time(Value, ValueStamp),
    ( I.VariableStamp >= ValueStamp ->  O = true; O = false ),
    mydebug('TimestampGreaterThanEquals'(out), (Variable, Value, I, O)).

'TimestampLessThan'(Variable, Value, I, O, _E) :-
    mydebug('TimestampLessThan'(in), (Variable, Value, I, O)),
    parse_time(Variable, VariableStamp),
    parse_time(Value, ValueStamp),
    ( I.VariableStamp < ValueStamp ->  O = true; O = false ),
    mydebug('TimestampLessThan'(out), (Variable, Value, I, O)).

'TimestampLessThanEquals'(Variable, Value, I, O, _E) :-
    mydebug('TimestampLessThanEquals'(in), (Variable, Value, I, O)),
    parse_time(Variable, VariableStamp),
    parse_time(Value, ValueStamp),
    ( I.VariableStamp =< ValueStamp ->  O = true; O = false ),
    mydebug('TimestampLessThanEquals'(out), (Variable, Value, I, O)).

%% Input and Output Processing
%% (TODO: full JsonPath syntax support)
process_input(OriginalInput, Input, Optional) :-
    option(input_path(InputPath), Optional)
    -> Input = OriginalInput.InputPath
    ;  Input = OriginalInput.

process_output(OriginalInput, Result, Output, Optional) :-
    ( option(result_path(ResultPath), Optional)
      -> I = OriginalInput.put(ResultPath, Result);  I = Result ),
    ( option(output_path(OutputPath), Optional)
      -> Output = I.OutputPath;  Output = I ).

%%
%% misc.
%%
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
:- begin_tests(process_io).

test(input, Input = _{val1:3, val2:4}) :-
    OriginalInput = _{title: "Numbers to add", numbers: _{val1:3, val2:4}},
    Optional = [input_path(numbers)],
    process_input(OriginalInput, Input, Optional).

test(output, Output = 7) :-
    OriginalInput = _{title: "Numbers to add", numbers: _{val1:3, val2:4}},
    Optional = [input_path(numbers), result_path(sum), output_path(sum)],
    process_output(OriginalInput, 7, Output, Optional).

:- end_tests(process_io).

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
