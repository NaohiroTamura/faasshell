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

mydebug(F, M) :- 
    thread_self(Id), thread_property(Id, id(N)),
    format("(~w): ~p~t~24| : ~p~n", [N, F, M]).

start(File, I, O) :-
    open(File, read, S),
    call_cleanup(
            read_term(S, Term, []),
            close(S)),
    mydebug(start(in), (Term, I, O)),
    assertz(Term),
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
    task_control(Action, Optional, I1, M1, E),
    ( option(retry(R), Optional), is_dict(M1), get_dict(error, M1, Error1)
      -> retry(Action, R, Optional, Error1, M2, E.put(_{action_input:I1}))
      ;  M2 = M1
    ),
    ( option(fallback(F), Optional), is_dict(M2), get_dict(error, M2, Error2)
      -> fallback(State, F, Error2, M3, E)
      ;  M3 = M2
    ),
    process_output(I, M3, O, Optional),
    mydebug(task(out), (I, O)).

error_code(time_limit_exceeded, _{error: "States.Timeout"}) :-
    mydebug(error_code, time_limit_exceeded), !.

error_code(error(timeout_error(_, _), _), _{error: "States.Timeout"}) :-
    mydebug(error_code, timeout_error), !.

error_code(heartbeat_error, _{error: "States.TaskFailed"}) :-
    mydebug(error_code, heartbeat_error), !.

error_code(Error, O) :-
    Error = error(permission_error(_, _), context(_, Status)), !,
    mydebug(error_code(permission(in)), (Status, O)),
    print_message(error, Error),
    O = _{error: "States.Permissions"},
    mydebug(error_code(permission(out)), (Status, O)).

error_code(Error, O) :-
    Error = error(existence_error(_, _), context(_, Status)), !,
    mydebug(error_code(existence_error(in)), (Status, O)),
    print_message(error, Error),
    O = _{error: "States.TaskFailed"},
    mydebug(error_code(existence_error(out)), (Status, O)).

error_code(Error, _{error: Error}) :-
    print_message(error, Error).

heartbeat(HeartBeatMBox, _TaskToken) :-
    mydebug(heartbeat, in),
    catch( ( repeat,
             %% TODO: get_heartbeat(TaskToken)
             sleep(5),
             %% 
             thread_send_message(HeartBeatMBox, heartbeat),
             false
           ),
           Error,
           mydebug(heartbeat(catch), Error)
         ).

heartbeat_message(stop, true) :- !,
    mydebug(task_heartbeat(repeat), stop).

heartbeat_message(Msg, false) :-
    mydebug(task_heartbeat(repeat), continue(Msg)).

task_heartbeat(TaskControlMBox, TaskToken, HeartbeatSeconds) :-
    mydebug(task_heartbeat(in), HeartbeatSeconds),
    %% TODO: token
    message_queue_create(HeartBeatMBox),
    thread_create(heartbeat(HeartBeatMBox, TaskToken), HeartBeatId),
    catch( ( ( repeat,
               mydebug(task_heartbeat(repeat), in),
               ( thread_get_message(HeartBeatMBox, Msg,
                                    [timeout(HeartbeatSeconds)])
                 -> heartbeat_message(Msg, Ret)
                 ;  ( mydebug(task_heartbeat(repeat), timeout(HeartbeatSeconds)),
                      %% task failed to send heartbeat within HeartbeatSeconds
                      error_code(heartbeat_error, O),
                      thread_send_message(TaskControlMBox, task_failed(O)),
                      Ret = true
                    )
               ),
               Ret
             ),
             mydebug(task_heartbeat(repeat), out)
           ),
           Error,
           mydebug(task_heartbeat(catch), Error)
         ),
    !,
    ( is_thread(HeartBeatId),
      thread_property(HeartBeatId, status(running))
      -> thread_signal(HeartBeatId, throw(heart_beat_kill)),
         thread_join(HeartBeatId, HeartBeatStatus),
         mydebug(task_heartbeat(out), heartbeat(HeartBeatStatus))
      ; true
    ),
    message_queue_destroy(HeartBeatMBox),
    mydebug(task_heartbeat(out), HeartBeatMBox).


task_control_message(task_completed(O), true, O) :- !,
    mydebug(task_control_message(task_completed(O)), true).

task_control_message(task_failed(O), true, O) :- !,
    mydebug(task_control_message(task_failed(O)), true).

task_control_message(Message, false, _) :-
    mydebug(task_control_message(Message), false).

task_control(Action, Optional, I, O, E) :-
    mydebug(task_control(in), (I, O)),
    message_queue_create(TaskControlMBox),
    %% TODO: generate TaskToken
    ( option(heartbeat_seconds(HeartbeatSeconds), Optional)
      -> thread_create(task_heartbeat(TaskControlMBox, TaskToken, HeartbeatSeconds),
                       TaskHeartBeatId)
      ;  TaskHeartBeatId = _
    ),
    thread_create(task_execute(Action,
                               [task_control_mbox(TaskControlMBox) | Optional],
                               I, O, E), TaskExecuteId),
    catch( ( ( repeat,
               mydebug(task_control(repeat(in)), (I, O)),
               ( thread_get_message(TaskControlMBox, Msg),
                 task_control_message(Msg, Ret, O)
               ),
               Ret
             ),
             mydebug(task_control(repeat(out)), (I, O))
           ),
           Error,
           mydebug(task_control(catch), Error)
         ),
    !,
    ( is_thread(TaskExecuteId),
      thread_property(TaskExecuteId, status(running))
      -> thread_signal(TaskExecuteId, throw(task_execute_kill)),
         thread_join(TaskExecuteId, TaskExecuteStatus),
         mydebug(task_control(cleanup), task_execute(TaskExecuteStatus))
      ;  true
    ),
    
    ( ground(TaskHeartBeatId),
      is_thread(TaskHeartBeatId),
      thread_property(TaskHeartBeatId, status(running))
      -> thread_signal(TaskHeartBeatId, throw(task_heartbeat_kill)),
         thread_join(TaskHeartBeatId, TaskHeartBeatStatus),
         mydebug(task_control(cleanup), task_heartbeat(TaskHeartBeatStatus))
      ;  true
    ),
    message_queue_destroy(TaskControlMBox),
    mydebug(task_control(out), (I, O)).


task_execute(Action, Optional, I, O, E) :-
    mydebug(task_execute(in), (I, O)),
    option(task_control_mbox(TaskControlMBox), Optional),
    option(timeout_seconds(TimeoutSeconds), Optional, infinite),
    mydebug(task_execute(timeout), TimeoutSeconds),
    WskApiEnv = [timeout(TimeoutSeconds) | E.openwhisk],
    catch( ( wsk_api_actions:invoke(Action, WskApiEnv, I, O),
             thread_send_message(TaskControlMBox, task_completed(O)),
             mydebug(task_execute(out), (I, O))
           ),
           Error,
           ( error_code(Error, O),
             mydebug(task_execute(catch), Error),
             ( Error = task_execute_kill
              -> true
              ;  thread_send_message(TaskControlMBox, task_failed(O))
             )
           )
         ).

retry(_Action, [], _Optional, O, O, _E) :-
    mydebug(retry(done), O).
retry(Action, [case(Cond, Params)|Cases], Optional, I, O, E) :-
    mydebug(task(retry(in)), (I, O)),
    reduce(Cond, I, M, E),
    (
        M == true
        -> mydebug(task(retry(true)), (case(Cond), I, O)),
           option(interval_seconds(IntervalSeconds), Params, 1),
           option(max_attempts(MaxAttempts), Params, 3),
           option(backoff_rate(BackoffRate), Params, 2.0),
           option(current_attempt(CurrentAttempt), Params, 0),
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
                merge_options([current_attempt(NewAttempt)], Params, NewParams),
                task_control(Action, Optional, E.action_input, M1, E),
                ( _{error: Err} :< M1 
                  -> mydebug(task(retry(again)), new_optional(NewParams)),
                     retry(Action, [case(Cond, NewParams)|Cases],
                           Optional, Err, O, E)
                  ;  retry(Action, [], Optional, M1, O, E)
                )
             ; retry(Action, [], Optional, _{error:I}, O, E)
           )
        ;  mydebug(task(retry(false)), (case(Cond), I, O)),
           retry(Action, Cases, Optional, I, O, E)
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
           fallback(State, Cases, I, O, E)
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

%% wait state
wait(State, seconds(Seconds), Optional, I, O, _E) :-
    mydebug(wait(in), (State, seconds(Seconds), I, O)),
    ( number(Seconds), Wait = Seconds;
      string(Seconds), number_string(Wait, Seconds)
    ),
    mydebug(wait(sleep), Wait),
    sleep(Wait),
    process_output(I, O, Optional),
    mydebug(wait(out), (State, I, O)).

wait(State, timestamp(Timestamp), Optional, I, O, _E) :-
    mydebug(wait(in), (State, timestamp(Timestamp), I, O)),
    parse_time(Timestamp, TargetStamp),
    get_time(CurrentStamp),
    Wait is TargetStamp - CurrentStamp,
    ( Wait > 0
      -> mydebug(wait(sleep), Wait),
         sleep(Wait)
      ;  mydebug(wait(nosleep), Wait),
         true
    ),
    process_output(I, O, Optional),
    mydebug(wait(out), (State, I, O)).

wait(State, seconds_path(SecondsPath), Optional, I, O, _E) :-
    mydebug(wait(in), (State, seconds_path(SecondsPath), I, O)),
    ( number(I.SecondsPath), Wait = I.SecondsPath;
      string(I.SecondsPath), number_string(Wait, I.SecondsPath)
    ),
    mydebug(wait(sleep), Wait),
    sleep(Wait),
    process_output(I, O, Optional),
    mydebug(wait(out), (State, I, O)).

wait(State, timestamp_path(TimestampPath), Optional, I, O, _E) :-
    mydebug(wait(in), (State, timestamp_path(TimestampPath), I, O)),
    parse_time(I.TimestampPath, TargetStamp),
    get_time(CurrentStamp),
    Wait is TargetStamp - CurrentStamp,
    ( Wait > 0
      -> mydebug(wait(sleep), Wait),
         sleep(Wait)
      ;  mydebug(wait(nosleep), Wait),
         true
    ),
    process_output(I, O, Optional),
    mydebug(wait(out), (State, I, O)).

%% fail state
fail(State, Optional, I, O, _E) :-
    mydebug(fail(in), (State, Optional, I, O)),
    (option(cause(Cause), Optional) -> O1 = I.put(cause, Cause); O1 = I),
    (option(error(Error), Optional) -> O2 = O1.put(error, Error); O2 = O1),
    O = O2,
    mydebug(fail(out), (State, I, O)).

%% parallel state
parallel(State, branches(Branches), Optional, I, O, E) :-
    mydebug(parallel(in), (State, Optional, I, O)),
    catch( ( process_input(I, I1, Optional),
             length(Branches, BL),
             length(Args, BL),
             maplist(=((I1,O1,E)), Args),
             mydebug(parallel(args), (O1,O)),
             concurrent_maplist(branch_execute, Branches, Args, Results),
             maplist([(_A,B,_C),B]>>true, Results, O2),
             mydebug(parallel(result), (O2, O)),
             process_output(I, O2, O, Optional)
           ),
           Error,
           ( mydebug(parallel(catch), Error),
             error_code(Error, O)
           )
        ),
    mydebug(parallel(out), (State, I, O)).

branch_execute(Branch, (I, O, E), (I, O, E)) :-
    mydebug(branch_execute(in), (I,O)),
    reduce(Branch, I, O, E),
    mydebug(branch_execute(out), (I,O)).

%% end of state
%%

%%
%% cyclic state transition
%%
goto(state(Target), I, O, E) :-
    mydebug(goto(in), (Target, I, O)),
    asl(States),
    setof(N,asl_run:lookup_state(Target,States,N),L),
    include(is_list, L, Next),
    length(Next,1),
    mydebug(goto(out), (Next, I, O)),
    reduce(Next, I, O, E).

lookup_state(Target, [State|States], Next) :-
    \+ is_list(State),   % writeln(s1(State)),
    State =.. [_, Target | _]
    -> Next = [State|States]
    ;  lookup_state(Target, States, Next).
lookup_state(Target, [Ss|Sss], Next) :-
    is_list(Ss),         % writeln(s2(Ss)),
    lookup_state(Target, Ss, Next)
    -> true
    ;  lookup_state(Target, Sss, Next).
lookup_state(Target, [parallel(_,branches(StatesList),_)|_], Next) :-
    % writeln(s3(StatesList)),
    lookup_state(Target, StatesList, Next).
lookup_state(_Target, [], nil).
        
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
    catch((I.Variable == Value ->  O = true; O = false ), _, O = false),
    mydebug('BooleanEquals'(out), (Variable, Value, I, O)).

'NumericEquals'(Variable, Value, I, O, _E) :-
    mydebug('NumericEquals'(in), (Variable, Value, I, O)),
    catch((I.Variable =:= Value ->  O = true; O = false), _, O = false),
    mydebug('NumericEquals'(out), (Variable, Value, I, O)).

'NumericGreaterThan'(Variable, Value, I, O, _E) :-
    mydebug('NumericGreaterThan'(in), (Variable, Value, I, O)),
    catch((I.Variable > Value ->  O = true; O = false ), _, O = false),
    mydebug('NumericGreaterThan'(out), (Variable, Value, I, O)).

'NumericGreaterThanEquals'(Variable, Value, I, O, _E) :-
    mydebug('NumericGreaterThanEquals'(in), (Variable, Value, I, O)),
    catch((I.Variable >= Value ->  O = true; O = false ), _, O = false),
    mydebug('NumericGreaterThanEquals'(out), (Variable, Value, I, O)).

'NumericLessThan'(Variable, Value, I, O, _E) :-
    mydebug('NumericLessThan'(in), (Variable, Value, I, O)),
    catch((I.Variable < Value ->  O = true; O = false ), _, O = false),
    mydebug('NumericLessThan'(out), (Variable, Value, I, O)).

'NumericLessThanEquals'(Variable, Value, I, O, _E) :-
    mydebug('NumericLessThanEquals'(in), (Variable, Value, I, O)),
    catch((I.Variable =< Value ->  O = true; O = false ), _, O = false),
    mydebug('NumericLessThanEquals'(out), (Variable, Value, I, O)).

'StringEquals'(Variable, Value, I, O, _E) :-
    mydebug('StringEquals'(in), (Variable, Value, I, O)),
    catch((I.Variable == Value ->  O = true; O = false ), _, O = false),
    mydebug('StringEquals'(out), (Variable, Value, I, O)).

'StringGreaterThan'(Variable, Value, I, O, _E) :-
    mydebug('StringGreaterThan'(in), (Variable, Value, I, O)),
    catch((I.Variable @> Value ->  O = true; O = false ), _, O = false),
    mydebug('StringGreaterThan'(out), (Variable, Value, I, O)).

'StringGreaterThanEquals'(Variable, Value, I, O, _E) :-
    mydebug('StringGreaterThanEquals'(in), (Variable, Value, I, O)),
    catch((I.Variable @>= Value ->  O = true; O = false ), _, O = false),
    mydebug('StringGreaterThanEquals'(out), (Variable, Value, I, O)).

'StringLessThan'(Variable, Value, I, O, _E) :-
    mydebug('StringLessThan'(in), (Variable, Value, I, O)),
    catch((I.Variable @< Value ->  O = true; O = false ), _, O = false),
    mydebug('StringLessThan'(out), (Variable, Value, I, O)).

'StringLessThanEquals'(Variable, Value, I, O, _E) :-
    mydebug('StringLessThanEquals'(in), (Variable, Value, I, O)),
    catch((I.Variable @=< Value ->  O = true; O = false ), _, O = false),
    mydebug('StringLessThanEquals'(out), (Variable, Value, I, O)).

'TimestampEquals'(Variable, Value, I, O, _E) :-
    mydebug('TimestampEquals'(in), (Variable, Value, I, O)),
    parse_time(Variable, VariableStamp),
    parse_time(Value, ValueStamp),
    catch((I.VariableStamp =:= ValueStamp ->  O = true; O = false ), _, O = false),
    mydebug('TimestampEquals'(out), (Variable, Value, I, O)).

'TimestampGreaterThan'(Variable, Value, I, O, _E) :-
    mydebug('TimestampGreaterThan'(in), (Variable, Value, I, O)),
    parse_time(Variable, VariableStamp),
    parse_time(Value, ValueStamp),
    catch((I.VariableStamp > ValueStamp ->  O = true; O = false ), _, O = false),
    mydebug('TimestampGreaterThan'(out), (Variable, Value, I, O)).

'TimestampGreaterThanEquals'(Variable, Value, I, O, _E) :-
    mydebug('TimestampGreaterThanEquals'(in), (Variable, Value, I, O)),
    parse_time(Variable, VariableStamp),
    parse_time(Value, ValueStamp),
    catch((I.VariableStamp >= ValueStamp ->  O = true; O = false ), _, O = false),
    mydebug('TimestampGreaterThanEquals'(out), (Variable, Value, I, O)).

'TimestampLessThan'(Variable, Value, I, O, _E) :-
    mydebug('TimestampLessThan'(in), (Variable, Value, I, O)),
    parse_time(Variable, VariableStamp),
    parse_time(Value, ValueStamp),
    catch((I.VariableStamp < ValueStamp ->  O = true; O = false ), _, O = false),
    mydebug('TimestampLessThan'(out), (Variable, Value, I, O)).

'TimestampLessThanEquals'(Variable, Value, I, O, _E) :-
    mydebug('TimestampLessThanEquals'(in), (Variable, Value, I, O)),
    parse_time(Variable, VariableStamp),
    parse_time(Value, ValueStamp),
    catch((I.VariableStamp =< ValueStamp ->  O = true; O = false ), _, O = false),
    mydebug('TimestampLessThanEquals'(out), (Variable, Value, I, O)).

%% Input and Output Processing
%% (TODO: full JsonPath syntax support)
process_input(OriginalInput, Input, Optional) :-
    var(Input),
    option(input_path(InputPath), Optional)
    -> Input = OriginalInput.InputPath
    ;  Input = OriginalInput.

process_output(Input, Output, Optional) :-
    var(Output),
    option(output_path(OutputPath), Optional)
    -> Output = Input.OutputPath
    ;  Output = Input.

process_output(OriginalInput, Result, Output, Optional) :-
    mydebug(process_output(in), (OriginalInput, Result, Output, Optional)),
    var(Output),
    catch( ( ( option(result_path(ResultPath), Optional)
               -> I = OriginalInput.put(ResultPath, Result)
               ;  I = OriginalInput.put(Result)
             ),
             mydebug(process_output(result_path), (I, ResultPath, Result, Output)),
             ( option(output_path(OutputPath), Optional)
               -> Output = I.OutputPath
               ;  Output = I
             ),
             mydebug(process_output(output_path), (Output, OutputPath))
           ),
           Error,
           ( mydebug(process_output(catch), Error),
             error_code(Error, Output)
           )
         ),
    mydebug(process_output(out), Output).

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

test(output, Output = _{a:1,prallel:[_{a:1},_{a:1}]}) :-
    OriginalInput = _{a:1},
    Optional = [result_path(parallel)],
    Result = [_{a:1},_{a:1}],
    process_output(OriginalInput, Result, Output, Optional).

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
