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

:- include('../../src/asl_run.pl').
:- include('unit_test_utils.pl').

%%
%% Unit Tests
%%
:- begin_tests(process_io).

test(input, Input = _{val1:3, val2:4}) :-
    OriginalInput = _{title: "Numbers to add", numbers: _{val1:3, val2:4}},
    Optional = [input_path('$.numbers')],
    process_input(OriginalInput, Input, Optional).

test(output, Output = 7) :-
    OriginalInput = _{title: "Numbers to add", numbers: _{val1:3, val2:4}},
    Optional = [input_path('$.numbers'), result_path('$.sum'), output_path('$.sum')],
    process_output(OriginalInput, 7, Output, Optional).

test(output, Output = _{a:1, parallel:[_{a:1},_{a:1}]}) :-
    OriginalInput = _{a:1},
    Optional = [result_path('$.parallel')],
    Result = [_{a:1}, _{a:1}],
    process_output(OriginalInput, Result, Output, Optional).

:- end_tests(process_io).

%%
:- begin_tests(pass).

test(hello, O = _{message:"Hello World!", name:"wsk"}) :-
    start('samples/wsk/dsl/hello_world.dsl', [], _{name:"wsk"}, O).

:- end_tests(pass).

%%
:- begin_tests(task,
               [setup(update_action("hello",
                                    'samples/wsk/actions/hello.js', "nodejs:6", []))
               ]).

test(hello, O = _{payload:"Hello, wsk!"}) :-
    start('samples/wsk/dsl/hello_world_task.dsl', [],  _{name:"wsk"}, O).

:- end_tests(task).

%%
:- begin_tests(choice,
               [setup(update_action("hello",
                                    'samples/wsk/actions/hello.js', "nodejs:6", []))
               ]).

test(case1, O = _{first_match_state:_, foo:1, next_state:_}) :-
    start('samples/wsk/dsl/choice_state.dsl', [], _{foo:1}, O).

test(case2, O = _{second_match_state:_, foo:2, next_state:_}) :-
    start('samples/wsk/dsl/choice_state.dsl', [], _{foo:2}, O).

test(default, O = _{cause:"No Matches!",error:"DefaultStateError"}) :-
    start('samples/wsk/dsl/choice_state.dsl', [], _{foo:5}, O).

test(casex1, O = _{public_state:_, type:"Public", next_state:_}) :-
    start('samples/wsk/dsl/choice_statex.dsl', [], _{type:"Public"}, O).

test(casex2, O = _{value_is_zero_state:_, type:"Private", value:0, next_state:_}) :-
    start('samples/wsk/dsl/choice_statex.dsl', [], _{type:"Private", value:0}, O).

test(casex3, O = _{value_in_twenties_state:_, type:"Private", value:25,
                   next_state:_}) :-
    start('samples/wsk/dsl/choice_statex.dsl', [], _{type:"Private", value:25}, O).

test(defaultx, O = _{cause:"No Matches!", error:null}) :-
    start('samples/wsk/dsl/choice_statex.dsl', [], _{type:"Private", value:35}, O).

:- end_tests(choice).

%%
:- begin_tests(wait,
               [setup(update_action("hello",
                                    'samples/wsk/actions/hello.js', "nodejs:6", []))
               ]).

test(normal, O = _{payload:"Hello, no sleep!"}) :-
    start('samples/wsk/dsl/wait_state.dsl', [],
          _{expirydate: "2017-09-04T01:59:00Z",
            expiryseconds:5, sleep:0, name:"no sleep"}, O).

:- end_tests(wait).

:- begin_tests(parallel).

test(normal, O = [_{var:1}, _{var:1}]) :-
    start('samples/wsk/dsl/parallel.dsl', [], _{var:1}, O).

:- end_tests(parallel).

:- begin_tests(job_status_poller,
               [setup(( update_action("hello",
                                      'samples/wsk/actions/hello.js', "nodejs:6", []),
                        update_action("job",
                                      'samples/wsk/actions/job.py', "python:2", [])))
              ]).

test(succeeded, O = _{payload:"Hello, Poller!"}) :-
    start('samples/wsk/dsl/job_status_poller.dsl', [],
          _{wait_time:1, params:["DEFAULT", "SUCCEEDED"], name:"Poller"}, O).

test(failed, O = _{cause:"AWS Batch Job Failed",
                   error:"DescribeJob returned FAILED"}) :-
    start('samples/wsk/dsl/job_status_poller.dsl', [],
          _{wait_time:1, params:["DEFAULT", "FAILED"], name:"Poller"}, O).

:- end_tests(job_status_poller).

:- begin_tests(task_timer,
               [setup(update_action("sns",
                                    'samples/wsk/actions/sns.py', "python:2", []))
               ]).

test(succeeded, Code = 200) :-
    start('samples/wsk/dsl/task_timer.dsl', [status_code(Code)],
          _{timer_seconds:1, status:"Sent"}, O),
    assertion(O = _{timer_seconds:1, status: "Sent"}).

test(failed, Code = 502 ) :-
    start('samples/wsk/dsl/task_timer.dsl', [status_code(Code)],
          _{timer_seconds:1, status:"ERROR"}, O),
    %% OpenWhisk Invoker / Python Runtime Issue regarding exception handling
    assertion(O = _{error:"The action did not return a dictionary."}).

:- end_tests(task_timer).

:- begin_tests(activity_task, [setup(mq_utils:mq_init)]).

test(activity_task_dsl_success, Status = true) :-
    message_queue_create(MQueue),
    thread_create(
            ( start('samples/wsk/dsl/activity_task.dsl',
                    [], _{name: "Activity"}, O),
              thread_send_message(MQueue, test_result(O))
            ),
            Id),
    Activity = "arn:aws:states:us-east-2:410388484666:activity:test",
    uuid(TaskToken),
    mq_utils:activity_start(Activity, TaskToken, InputText),
    atom_json_dict(InputText, Input, []),
    assertion(Input = _{name: "Activity"}),

    mq_utils:activity_heartbeat(Activity, TaskToken),

    atomics_to_string(["Hello, ", Input.name, "!"], Output),
    atom_json_dict(OutputText, _{payload: Output}, []),
    mq_utils:activity_end(Activity, TaskToken, success, OutputText),

    thread_join(Id, Status),
    thread_get_message(MQueue, test_result(O)),
    assertion(O = _{payload:"Hello, Activity!"}).

test(activity_task_dsl_failure, Status = true) :-
    message_queue_create(MQueue),
    thread_create(
            ( start('samples/wsk/dsl/activity_task.dsl',
                    [], _{comment: "failure test"}, O),
              thread_send_message(MQueue, test_result(O))
            ),
            Id),
    Activity = "arn:aws:states:us-east-2:410388484666:activity:test",
    uuid(TaskToken),
    mq_utils:activity_start(Activity, TaskToken, InputText),
    atom_json_dict(InputText, Input, []),
    assertion(Input = _{comment: "failure test"}),

    catch( atomics_to_string(["Hello, ", Input.name, "!"], _Output),
           error(existence_error(Key ,Name,_),_),
           true),

    mq_utils:activity_heartbeat(Activity, TaskToken),

    format(string(Cause), "~w '~w' doesn't exist", [Key, Name]),
    atom_json_dict(OutputText, _{error: "existence_error", cause: Cause}, []),
    mq_utils:activity_end(Activity, TaskToken, failure, OutputText),

    thread_join(Id, Status),
    thread_get_message(MQueue, test_result(O)),
    assertion(O = _{error: "existence_error", cause: "key 'name' doesn't exist"}).

test(activity_task_timeout_dsl) :-
    start('samples/wsk/dsl/activity_task_timeout.dsl', [],
          _{name: "Activity Timeout"}, O),
    assertion(O = _{error:"States.Timeout"}).

test(activity_task_heartbeat_dsl_success, Status = true) :-
    message_queue_create(MQueue),
    thread_create(
            ( start('samples/wsk/dsl/activity_task_heartbeat.dsl',
                    [], _{name: "Activity"}, O),
              thread_send_message(MQueue, test_result(O))
            ),
            Id),
    Activity = "arn:aws:states:us-east-2:410388484666:activity:test",
    uuid(TaskToken),
    mq_utils:activity_start(Activity, TaskToken, InputText),
    atom_json_dict(InputText, Input, []),
    assertion(Input = _{name: "Activity"}),
    sleep(2),

    mq_utils:activity_heartbeat(Activity, TaskToken),
    sleep(2),

    mq_utils:activity_heartbeat(Activity, TaskToken),
    sleep(2),

    atomics_to_string(["Hello, ", Input.name, "!"], Output),
    atom_json_dict(OutputText, _{payload: Output}, []),
    mq_utils:activity_end(Activity, TaskToken, success, OutputText),

    thread_join(Id, Status),
    thread_get_message(MQueue, test_result(O)),
    assertion(O = _{payload:"Hello, Activity!"}).

test(activity_task_heartbeat_dsl_hearbeat_timeout, Status = true) :-
    message_queue_create(MQueue),
    thread_create(
            ( start('samples/wsk/dsl/activity_task_heartbeat.dsl',
                    [], _{name: "Activity"}, O),
              thread_send_message(MQueue, test_result(O))
            ),
            Id),
    Activity = "arn:aws:states:us-east-2:410388484666:activity:test",
    uuid(TaskToken),
    mq_utils:activity_start(Activity, TaskToken, InputText),
    atom_json_dict(InputText, Input, []),
    assertion(Input = _{name: "Activity"}),
    sleep(2),

    mq_utils:activity_heartbeat(Activity, TaskToken),
    sleep(4),

    thread_join(Id, Status),
    thread_get_message(MQueue, test_result(O)),
    assertion(O = _{error:"States.Timeout", cause:"heartbeat timeout"}).

test(activity_task_timeout_heartbeat_dsl_timeout, Status = true) :-
    message_queue_create(MQueue),
    thread_create(
            ( start('samples/wsk/dsl/activity_task_timeout_heartbeat.dsl',
                    [], _{name: "Activity"}, O),
              thread_send_message(MQueue, test_result(O))
            ),
            Id),
    uuid(TaskToken),
    Activity = "arn:aws:states:us-east-2:410388484666:activity:test",
    mq_utils:activity_start(Activity, TaskToken, InputText),
    atom_json_dict(InputText, Input, []),
    assertion(Input = _{name: "Activity"}),
    sleep(1),

    mq_utils:activity_heartbeat(Activity, TaskToken),
    sleep(2),

    mq_utils:activity_heartbeat(Activity, TaskToken),
    sleep(2),

    thread_join(Id, Status),
    thread_get_message(MQueue, test_result(O)),
    assertion(O = _{error:"States.Timeout"}).

:- end_tests(activity_task).
