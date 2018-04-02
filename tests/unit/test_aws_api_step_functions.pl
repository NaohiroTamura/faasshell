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

:- include('../../src/aws_api_step_functions.pl').

%%
%% Unit Tests
%%
:- use_module(library(plunit)).

personal(Region, Account) :-
    getenv('aws_region', Region),
    getenv('aws_account_id', Account).

:- begin_tests(invalid_definition).

test(create, Code = 400) :-
    setup_call_cleanup(
            open('samples/aws/asl/has-dupes.json', read, S),
            read_string(S, _N, ASL),
            close(S)),
    personal(Region, Account),
    atomic_list_concat(
            ['arn:aws:iam::', Account, ':role/service-role/StatesExecutionRole-',
             Region], ARN1),
    Request = _{'roleArn': ARN1, definition: ASL},
    atomic_list_concat(
            [arn, aws, states, Region, Account, stateMachine, 'InvalidDefinition'],
            ':', ARN2),
    aws_api_step_functions:create_statemachine(ARN2, [status_code(Code)],
                                               Request, R),
    assertion(_{message:"Invalid State Machine Definition: 'DUPLICATE_STATE_NAME at /'"} :<R).

test(delete, Code = 200) :-
    personal(Region, Account),
    atomic_list_concat(
            [arn, aws, states, Region, Account, stateMachine, 'Activitytask0'],
            ':', ARN),
    aws_api_step_functions:delete_statemachine(ARN, [status_code(Code)], R),
    assertion(R = _{}).

:- end_tests(invalid_definition).

%%
:- begin_tests(statemachine).

test(describe, Code = 200) :-
    personal(Region, Account),
    atomic_list_concat(
            [arn, aws, states, Region, Account, stateMachine, 'Activitytask'],
            ':', ARN),
    aws_api_step_functions:describe_statemachine(ARN, [status_code(Code)], R),
    assertion(R.name = "Activitytask").

test(start_stop, (Code1, Code2) = (200, 200)) :-
    personal(Region, Account),
    atomic_list_concat(
            [arn, aws, states, Region, Account, stateMachine, 'Activitytask'],
            ':', ARN),

    Input = _{comments: "start & stop unit test"},
    aws_api_step_functions:start_execution(ARN, [status_code(Code1)], Input, R1),
    %% assertion dosn't bind value
    assertion(_{executionArn: _,  startDate: _} :< R1),
    sleep(3),

    Request2 = _{cause: "stop execution api is called",
                 error: "statemachine unit test",
                 executionArn: R1.executionArn},
    aws_api_step_functions:stop_execution(ARN, [status_code(Code2)], Request2, R2),
    assertion(_{stopDate: _} :< R2).

:- end_tests(statemachine).

%%
:- begin_tests(activity_task).

test(succeed, (Code0, Code1, Code2, Code3)  = (200, 200, 200, 200)) :-
    personal(Region, Account),
    atomic_list_concat(
            [arn, aws, states, Region, Account, stateMachine, 'Activitytask'],
            ':', ARN1),
    aws_api_step_functions:start_execution(ARN1, [status_code(Code0)],
                                           _{name: "Activity"}, R0),
    assertion(_{executionArn: _,  startDate: _} :< R0),
    sleep(1),

    atomic_list_concat(
            [arn, aws, states, Region, Account, activity, test],
            ':', ARN2),
    aws_api_step_functions:get_activity_task(ARN2, [status_code(Code1)], R1),
    assertion(_{taskToken: _, input: _} :< R1),
    atom_json_dict(R1.input, Input, []),
    sleep(1),

    aws_api_step_functions:send_task_heartbeat(ARN2, [status_code(Code2)],
                                               R1.taskToken, R2),
    assertion(R2 = _{}),
    sleep(1),

    format(string(Output), '"Hello, ~w!"', [Input.name]),
    aws_api_step_functions:send_task_success(ARN2, [status_code(Code3)],
        _{output: Output, taskToken: R1.taskToken}, R3),
    assertion(R3 = _{}).

test(failure, (Code0, Code1, Code2, Code3)  = (200, 200, 200, 200)) :-
    personal(Region, Account),
    atomic_list_concat(
            [arn, aws, states, Region, Account, stateMachine, 'Activitytask'],
            ':', ARN1),
    aws_api_step_functions:start_execution(ARN1,
        [status_code(Code0)], _{name: "Activity"}, R0),
    assertion(_{executionArn: _,  startDate: _} :< R0),
    sleep(1),

    atomic_list_concat(
            [arn, aws, states, Region, Account, activity, test],
            ':', ARN2),
    aws_api_step_functions:get_activity_task(ARN2, [status_code(Code1)], R1),
    assertion(_{taskToken: _, input: _} :< R1),
    sleep(1),

    aws_api_step_functions:send_task_heartbeat(ARN2, [status_code(Code2)],
                                               R1.taskToken, R2),
    assertion(R2 = _{}),
    sleep(1),

    aws_api_step_functions:send_task_failure(ARN2, [status_code(Code3)],
        _{cause: "send task failure api is called",
          error: "activity task unit test",
          taskToken: R1.taskToken}, R3),
    assertion(R3 = _{}).

test(invalid_activity, Code = 400) :-
    personal(Region, Account),
    atomic_list_concat(
            [arn, aws, states, Region, Account, activity, fake],
            ':', ARN),
    aws_api_step_functions:get_activity_task(ARN, [status_code(Code)], R),
    assertion(_{'__type':"com.amazonaws.swf.service.v2.model#ActivityDoesNotExist",
                message: _} :< R).

test(invalid_token_and_invalid_output,
     (Code0, Code1, Code2, Code3, Code4) = (200, 200, 400, 400, 200)) :-
    personal(Region, Account),
    atomic_list_concat(
            [arn, aws, states, Region, Account, stateMachine, 'Activitytask'],
            ':', ARN1),
    aws_api_step_functions:start_execution(ARN1, [status_code(Code0)],
                                           _{name: "Activity"}, R0),
    assertion(_{executionArn: _,  startDate: _} :< R0),
    sleep(1),

    atomic_list_concat(
            [arn, aws, states, Region, Account, activity, test],
            ':', ARN2),
    aws_api_step_functions:get_activity_task(ARN2, [status_code(Code1)], R1),
    assertion(_{taskToken: _, input: _} :< R1),
    atom_json_dict(R1.input, Input, []),
    sleep(1),

    aws_api_step_functions:send_task_heartbeat(ARN2, [status_code(Code2)],
                                               "FakeTaskToken", R2),
    assertion(R2 = _{'__type':"com.amazonaws.swf.service.v2.model#InvalidToken",
                     message:"Invalid Token: 'Invalid token'"}),
    sleep(1),

    format(string(Output), '"Hello, ~w!"', [Input.name]),
    aws_api_step_functions:send_task_success(ARN2, [status_code(Code3)],
        _{fake: Output, taskToken: R1.taskToken}, R3),
    assertion(R3 = _{'__type':"com.amazon.coral.validate#ValidationException",
                     message:"1 validation error detected: Value at 'output' failed to satisfy constraint: Member must not be null"}),

    aws_api_step_functions:stop_execution(ARN2, [status_code(Code4)],
        _{cause: "send stop execution api is called",
                                error: "invalid token and invalid output test",
                                executionArn: R0.executionArn}, R4),
    assertion(R4 = _{stopDate: _}).

:- end_tests(activity_task).
