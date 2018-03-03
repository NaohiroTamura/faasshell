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
%% Amazon State Language (ASL) DSL Interpreter
%%

:- module(faasshell_run,
          [ start/4
         ]).

:- use_module(json_utils).
:- use_module(mq_utils).
:- use_module(wsk_api_utils).
:- use_module(wsk_api_actions, [faas:invoke/4]).
:- use_module(aws_api_lambda, [faas:invoke/4]).
:- use_module(gcp_api_functions, [faas:invoke/4]).
:- use_module(azure_api_functions, [faas:invoke/4]).

:- use_module(library(http/json)).
:- use_module(library(http/http_log)).

/*******************************
 *   PLUGIN FOR FaaS API       *
 *******************************/
:- multifile
       faas:invoke/4.

mydebug(F, M) :- 
    thread_self(Id), thread_property(Id, id(N)),
    %% format("(~w): ~p~t~24| : ~p~n", [N, F, M]).
    http_log("(~w): ~p~t~24| : ~p~n", [N, F, M]).

start(File, Options, I, O) :-
    ( atom(File); string(File) ), !,
    set_setting(http:logfile,'/logs/httpd.log'), % docker volume /tmp
    setup_call_cleanup(
            open(File, read, S),
            read_term(S, Term, []),
            close(S)),
    start(Term, Options, I, O).

start(Term, Options, I, O) :-
    Term = asl(Dsl), !,
    mydebug(start(in), (Term, I, O)),
    reduce(Term, I, O, _{faas: Options, asl: Dsl}),
    mydebug(start(out), (I, O)).
              
%%
%% begin of iterpreter
%%
%% reduce(+Dsl, +Input, -Output, +Environment)
reduce(asl(Dsl), I, O, E) :- 
    !,
    mydebug(reduce(asl(in)), (I, O)),
    reduce(Dsl, I, O, E),
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
    task_execute(Action, Optional, I1, M1, E),
    ( option(retry(R), Optional), is_dict(M1), get_dict(error, M1, Error1)
      -> retry(task_execute(Action, Optional, I1), R, Error1, M2, E)
      ;  M2 = M1
    ),
    ( option(fallback(F), Optional), is_dict(M2), get_dict(error, M2, Error2)
      -> fallback(State, F, Error2, M3, E)
      ;  M3 = M2
    ),
    process_output(I, M3, O, Optional),
    mydebug(task(out), (I, O)).

activity_task_heartbeat(ActivityTaskId, Action, TaskToken,
                        HeartbeatSeconds) :-
    mydebug(activity_task_heartbeat(in), (Action, TaskToken,
                                          HeartbeatSeconds)),
    ( repeat,
      ( mq_utils:activity_heartbeated(Action, TaskToken, HeartbeatSeconds)
        -> Ret = fail
        ;  thread_signal(ActivityTaskId, throw(heartbeat_timeout)),
           Ret = true
      ),
      Ret
    ),
    mydebug(activity_task_heartbeat(out), (Action, TaskToken,
                                           HeartbeatSeconds)).

activity_task(Action, Optional, I, O, _E) :-
    mydebug(activity_task(in), (Action, I, O)),

    atom_json_dict(InputText, I, []),
    mq_utils:activity_started(Action, InputText, TaskToken),

    %% ASL spec defines the default timeout value is 99999999
    option(heartbeat_seconds(HeartbeatSeconds), Optional, 99999999),
    mydebug(activity_task, heartbeat_seconds(HeartbeatSeconds)),
    thread_self(ActivityTaskId),
    thread_create(
            activity_task_heartbeat(ActivityTaskId, Action, TaskToken,
                                    HeartbeatSeconds),
            HeartbeatId),

    catch( ( mq_utils:activity_ended(Action, TaskToken, _Result, OutputText),
             atom_json_dict(OutputText, O, [])
            ),
            Error,
            ( error_code(Error, O),
              mydebug(activity_task(catch), Error),
              ( Error = heartbeat_timeout
                -> thread_join(HeartbeatId, HeartbeatStatus),
                   mydebug(activity_task(heartbeat_timeout_cleanup),
                           heartbeat(HeartbeatStatus))
                ; true
              )
            )
         ),

    ( is_thread(HeartbeatId),
      thread_property(HeartbeatId, status(running))
      -> thread_signal(HeartbeatId, throw(heartbeat_kill)),
         thread_join(HeartbeatId, HeartbeatStatus),
         mydebug(activity_task(hearbeat_kill_cleanup), heartbeat(HeartbeatStatus))
      ;  true
    ),
    mydebug(activity_task(out), (I, O)).

task_execute(Action, Optional, I, O, E) :-
    mydebug(task_execute(in), (I, O)),
    catch( ( atomic_list_concat([_, _, states, _, _, activity, _], ':', Action)
             -> %% process activity
                %% ASL spec defines the default timeout value is 99999999
                option(timeout_seconds(TimeoutSeconds), Optional, 99999999),
                mydebug(task_execute(activity), timeout_seconds(TimeoutSeconds)),
                call_with_time_limit(TimeoutSeconds,
                                     activity_task(Action, Optional, I, O, E))
             ;  %% process function, call faas plugin
                %% http_open causes the following error if timeout is 99999999
                %% "SSL(SSL_eof) negotiate: Unexpected end-of-file".
                option(timeout_seconds(TimeoutSeconds), Optional, infinite),
                mydebug(task_execute(function), timeout_seconds(TimeoutSeconds)),
                ApiEnv = [timeout(TimeoutSeconds) | E.faas],
                faas:invoke(Action, ApiEnv, I, O)
           ),
           Error,
           ( error_code(Error, O),
             mydebug(task_execute(catch), Error)
           )
         ),
    mydebug(task_execute(out), (I, O)).

retry(_PartialFunc, [], O, O, _E) :-
    mydebug(retry(done), O).
retry(PartialFunc, [case(Cond, Params)|Cases], I, O, E) :-
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
                PartialFunc =.. CL1, append(CL1, [M1, E], CL2),
                RetryCommand =.. CL2, RetryCommand,
                ( _{error: Err} :< M1 
                  -> mydebug(task(retry(again)), new_optional(NewParams)),
                     retry(PartialFunc, [case(Cond, NewParams)|Cases], Err, O, E)
                  ;  retry(PartialFunc, [], M1, O, E)
                )
             ; retry(PartialFunc, [], _{error:I}, O, E)
           )
        ;  mydebug(task(retry(false)), (case(Cond), I, O)),
           retry(PartialFunc, Cases, I, O, E)
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
           reduce(States, _{error: I}, O, E)
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
    json_utils:json_path_value(SecondsPath, I, _K, _R, WaitValue),
    mydebug(wait(wait_value), (seconds_path(SecondsPath), WaitValue)),
    ( number(WaitValue), Wait = WaitValue;
      string(WaitValue), number_string(Wait, WaitValue)
    ),
    mydebug(wait(sleep), Wait),
    sleep(Wait),
    process_output(I, O, Optional),
    mydebug(wait(out), (State, I, O)).

wait(State, timestamp_path(TimestampPath), Optional, I, O, _E) :-
    mydebug(wait(in), (State, timestamp_path(TimestampPath), I, O)),
    json_utils:json_path_value(TimestampPath, I, _K, _R, TimestampValue),
    parse_time(TimestampValue, TargetStamp),
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

%% succeed state
succeed(State, Optional, I, O, _E) :-
    mydebug(succeed(in), (State, Optional, I, O)),
    process_output(I, O, Optional),
    mydebug(succeed(out), (State, I, O)).

%% fail state
fail(State, Optional, I, O, _E) :-
    mydebug(fail(in), (State, Optional, I, O)),
    option(cause(Cause), Optional, null),
    O1 = _{cause: Cause},
    option(error(Error), Optional, null),
    O = O1.put(error, Error),
    mydebug(fail(out), (State, I, O)).

%% parallel state
parallel(State, branches(Branches), Optional, I, O, E) :-
    mydebug(parallel(in), (State, Optional, I, O)),
    process_input(I, I1, Optional),
    parallel_execute(Branches, I1, M1, E),
    ( option(retry(R), Optional), is_dict(M1), get_dict(error, M1, Error1)
      -> retry(parallel_execute(Branches, I), R, Error1, M2, E)
      ;  M2 = M1
    ),
    ( option(fallback(F), Optional), is_dict(M2), get_dict(error, M2, Error2)
      -> fallback(State, F, Error2, M3, E)
      ;  M3 = M2
    ),
    process_output(I, M3, O, Optional),
    mydebug(parallel(out), (State, I, O)).

parallel_execute(Branches, I, O, E) :-
    mydebug(parallel_execute(in), (I, O)),
    catch( ( 
             length(Branches, BL),
             length(Args, BL),
             maplist(=((I,M,E)), Args),
             mydebug(parallel(args), (M,O)),
             concurrent_maplist(branch_execute, Branches, Args, Results),
             maplist([(_A,B,_C),B]>>true, Results, O), %% O is list
             mydebug(parallel_execute(result), O)
           ),
           Error,
           ( mydebug(parallel(catch), Error),
             error_code(Error, O)
           )
        ),
    mydebug(parallel_execute(out), (I, O)).

branch_execute(Branch, (I, O, E), (I, O, E)) :-
    mydebug(branch_execute(in), (I,O)),
    reduce(Branch, I, O, E),
    mydebug(branch_execute(out), (I,O)),
    ( is_dict(O), get_dict(error, O, Error)
      -> throw(Error)
      ;  true
    ).

%% end of state
%%

%%
%% cyclic state transition
%%
goto(state(Target), I, O, E) :-
    mydebug(goto(in), (Target, I, O)),
    States = E.asl,
    setof(N, faasshell_run:lookup_state(Target,States,N),Next),
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
    lookup_state(Target, Ss, Next);
    lookup_state(Target, Sss, Next).
lookup_state(Target, [parallel(_,branches(StatesList),_)|_], Next) :-
    % writeln(s3(StatesList)),
    lookup_state(Target, StatesList, Next).
lookup_state(_Target, [], _) :- fail.
        
%%
%% retry and fallback conditions
%%
'ErrorEquals'(["States.ALL"], I, true, _E) :- !,
    mydebug('ErrorEquals'("States.ALL"), (I, true)).
'ErrorEquals'(["States.TaskFailed"], I, O, _E) :- !,
    mydebug('ErrorEquals'(in), ("States.TaskFailed", I, O)),
    (re_match("Error"/i, I) -> O = true; O = false),
    mydebug('ErrorEquals'(out), ("States.TaskFailed", I, O)).
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
    catch((json_utils:json_path_value(Variable, I, _K, _R, V),
           V == Value ->  O = true; O = false ), _, O = false),
    mydebug('BooleanEquals'(out), (Variable, Value, I, O)).

'NumericEquals'(Variable, Value, I, O, _E) :-
    mydebug('NumericEquals'(in), (Variable, Value, I, O)),
    catch((json_utils:json_path_value(Variable, I, _K, _R, V),
           V =:= Value ->  O = true; O = false), _, O = false),
    mydebug('NumericEquals'(out), (Variable, Value, I, O)).

'NumericGreaterThan'(Variable, Value, I, O, _E) :-
    mydebug('NumericGreaterThan'(in), (Variable, Value, I, O)),
    catch((json_utils:json_path_value(Variable, I, _K, _R, V),
           V > Value ->  O = true; O = false ), _, O = false),
    mydebug('NumericGreaterThan'(out), (Variable, Value, I, O)).

'NumericGreaterThanEquals'(Variable, Value, I, O, _E) :-
    mydebug('NumericGreaterThanEquals'(in), (Variable, Value, I, O)),
    catch((json_utils:json_path_value(Variable, I, _K, _R, V),
           V >= Value ->  O = true; O = false ), _, O = false),
    mydebug('NumericGreaterThanEquals'(out), (Variable, Value, I, O)).

'NumericLessThan'(Variable, Value, I, O, _E) :-
    mydebug('NumericLessThan'(in), (Variable, Value, I, O)),
    catch((json_utils:json_path_value(Variable, I, _K, _R, V),
           V < Value ->  O = true; O = false ), _, O = false),
    mydebug('NumericLessThan'(out), (Variable, Value, I, O)).

'NumericLessThanEquals'(Variable, Value, I, O, _E) :-
    mydebug('NumericLessThanEquals'(in), (Variable, Value, I, O)),
    catch((json_utils:json_path_value(Variable, I, _K, _R, V),
           V =< Value ->  O = true; O = false ), _, O = false),
    mydebug('NumericLessThanEquals'(out), (Variable, Value, I, O)).

'StringEquals'(Variable, Value, I, O, _E) :-
    mydebug('StringEquals'(in), (Variable, Value, I, O)),
    catch((json_utils:json_path_value(Variable, I, _K, _R, V),
           V == Value ->  O = true; O = false ), _, O = false),
    mydebug('StringEquals'(out), (Variable, Value, I, O)).

'StringGreaterThan'(Variable, Value, I, O, _E) :-
    mydebug('StringGreaterThan'(in), (Variable, Value, I, O)),
    catch((json_utils:json_path_value(Variable, I, _K, _R, V),
           V @> Value ->  O = true; O = false ), _, O = false),
    mydebug('StringGreaterThan'(out), (Variable, Value, I, O)).

'StringGreaterThanEquals'(Variable, Value, I, O, _E) :-
    mydebug('StringGreaterThanEquals'(in), (Variable, Value, I, O)),
    catch((json_utils:json_path_value(Variable, I, _K, _R, V),
           V @>= Value ->  O = true; O = false ), _, O = false),
    mydebug('StringGreaterThanEquals'(out), (Variable, Value, I, O)).

'StringLessThan'(Variable, Value, I, O, _E) :-
    mydebug('StringLessThan'(in), (Variable, Value, I, O)),
    catch((json_utils:json_path_value(Variable, I, _K, _R, V),
           V @< Value ->  O = true; O = false ), _, O = false),
    mydebug('StringLessThan'(out), (Variable, Value, I, O)).

'StringLessThanEquals'(Variable, Value, I, O, _E) :-
    mydebug('StringLessThanEquals'(in), (Variable, Value, I, O)),
    catch((json_utils:json_path_value(Variable, I, _K, _R, V),
           V @=< Value ->  O = true; O = false ), _, O = false),
    mydebug('StringLessThanEquals'(out), (Variable, Value, I, O)).

'TimestampEquals'(Variable, Value, I, O, _E) :-
    mydebug('TimestampEquals'(in), (Variable, Value, I, O)),
    catch((json_utils:json_path_value(Variable, I, _K, _R, V),
           parse_time(V, VariableStamp),
           parse_time(Value, ValueStamp),
           VariableStamp =:= ValueStamp ->  O = true; O = false ), _, O = false),
    mydebug('TimestampEquals'(out), (Variable, Value, I, O)).

'TimestampGreaterThan'(Variable, Value, I, O, _E) :-
    mydebug('TimestampGreaterThan'(in), (Variable, Value, I, O)),
    catch((json_utils:json_path_value(Variable, I, _K, _R, V),
           parse_time(V, VariableStamp),
           parse_time(Value, ValueStamp),
           VariableStamp > ValueStamp ->  O = true; O = false ), _, O = false),
    mydebug('TimestampGreaterThan'(out), (Variable, Value, I, O)).

'TimestampGreaterThanEquals'(Variable, Value, I, O, _E) :-
    mydebug('TimestampGreaterThanEquals'(in), (Variable, Value, I, O)),
    catch((json_utils:json_path_value(Variable, I, _K, _R, V),
           parse_time(V, VariableStamp),
           parse_time(Value, ValueStamp),
           VariableStamp >= ValueStamp ->  O = true; O = false ), _, O = false),
    mydebug('TimestampGreaterThanEquals'(out), (Variable, Value, I, O)).

'TimestampLessThan'(Variable, Value, I, O, _E) :-
    mydebug('TimestampLessThan'(in), (Variable, Value, I, O)),
    catch((json_utils:json_path_value(Variable, I, _K, _R, V),
           parse_time(V, VariableStamp),
           parse_time(Value, ValueStamp),
           VariableStamp < ValueStamp ->  O = true; O = false ), _, O = false),
    mydebug('TimestampLessThan'(out), (Variable, Value, I, O)).

'TimestampLessThanEquals'(Variable, Value, I, O, _E) :-
    mydebug('TimestampLessThanEquals'(in), (Variable, Value, I, O)),
    catch((json_utils:json_path_value(Variable, I, _K, _R, V),
           parse_time(V, VariableStamp),
           parse_time(Value, ValueStamp),
           VariableStamp =< ValueStamp ->  O = true; O = false ), _, O = false),
    mydebug('TimestampLessThanEquals'(out), (Variable, Value, I, O)).

%% Input and Output Processing
process_input(OriginalInput, Input, Optional) :-
    mydebug(process_input3(in), (OriginalInput, Input, Optional)),
    var(Input),
    ( option(input_path(InputPath), Optional)
      -> ( InputPath = null
           -> Input = _{}
           ;  json_utils:json_path_value(InputPath, OriginalInput, _K, _R, Input)
         )
      ;  Input = OriginalInput
    ),
    mydebug(process_input3(out), Input).

process_output(Input, Output, Optional) :-
    mydebug(process_output3(in), (Input, Output, Optional)),
    var(Output),
    ( option(output_path(OutputPath), Optional)
      -> json_utils:json_path_value(OutputPath, Input, _K, _R, Output)
      ;  Output = Input
    ),
    mydebug(process_output3(out), Output).

process_output(OriginalInput, Result, Output, Optional) :-
    mydebug(process_output4(in), (OriginalInput, Result, Output, Optional)),
    var(Output),
    catch( ( ( option(result_path(ResultPath), Optional)
               -> % AWS Lambda function can return not only dict, but also value.
                  % However OpenWhisk action can retrun only dict.
                  json_utils:json_path_merge(ResultPath, OriginalInput,
                                             Result, _K1, I)
               ;  % ResultPath has the default value of $
                  % The type of Result from Parallel state is List.
                  I = Result
             ),
             mydebug(process_output(result_path), (I, ResultPath, Result, Output)),
             ( option(output_path(OutputPath), Optional)
               -> json_utils:json_path_value(OutputPath, I, _K2, _R, Output)
               ;  Output = I
             ),
             mydebug(process_output(output_path), (Output, OutputPath))
           ),
           Error,
           ( mydebug(process_output(catch), Error),
             error_code(Error, Output)
           )
         ),
    mydebug(process_output4(out), Output).

%%
%% error code
%%
error_code(time_limit_exceeded, _{error: "States.Timeout"}) :-
    mydebug(error_code, time_limit_exceeded), !.

error_code(error(timeout_error(_, _), _), _{error: "States.Timeout"}) :-
    mydebug(error_code, timeout_error), !.

error_code(heartbeat_timeout, _{error: "States.Timeout",
                                cause: "heartbeat timeout"}) :-
    mydebug(error_code, heartbeat_timeout), !.

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

error_code(Error, O) :-
    Error = error(type_error(_, _), context(_, Status)), !,
    mydebug(error_code(type_error(in)), (Status, O)),
    print_message(error, Error),
    O = _{error: "States.TaskFailed"},
    mydebug(error_code(type_error(out)), (Status, O)).

error_code(heartbeat_kill, _) :-
    print_message(warning, format('caught heartbeat_kill message', [])).

error_code(Error, _{error: Error}) :-
    print_message(error, Error).

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
