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


:- begin_tests(invalid_definition).

test(create, Code = 400) :-
    open('samples/aws/asl/has-dupes.json', read, S),
    call_cleanup(
            read_string(S, _N, ASL),
            close(S)),
    Request = _{'roleArn': 'arn:aws:iam::410388484666:role/service-role/StatesExecutionRole-us-east-2',
                definition: ASL},
    aws_api_step_functions:create_statemachine(
        'arn:aws:states:us-east-2:410388484666:stateMachine:InvalidDefinition',
        [status_code(Code)], Request, R),
    assertion(_{message:"Invalid State Machine Definition: 'DUPLICATE_STATE_NAME at /'"} :<R).

test(delete, Code = 200) :-
    aws_api_step_functions:delete_statemachine(
        'arn:aws:states:us-east-2:410388484666:stateMachine:Activitytask0',
        [status_code(Code)], R),
    assertion(R = _{}).

:- end_tests(invalid_definition).

%%
:- begin_tests(statemachine).

test(describe, Code = 200) :-
    aws_api_step_functions:describe_statemachine(
        'arn:aws:states:us-east-2:410388484666:stateMachine:Activitytask',
        [status_code(Code)], R),
    assertion(R.name = "Activitytask").

test(start_stop, (Code1, Code2) = (200, 200)) :-
    ARN = 'arn:aws:states:us-east-2:410388484666:stateMachine:Activitytask',

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
    aws_api_step_functions:start_execution(
        'arn:aws:states:us-east-2:410388484666:stateMachine:Activitytask',
        [status_code(Code0)], _{name: "Activity"}, R0),
    assertion(_{executionArn: _,  startDate: _} :< R0),
    sleep(1),

    aws_api_step_functions:get_activity_task(
        'arn:aws:states:us-east-2:410388484666:activity:test',
        [status_code(Code1)], R1),
    assertion(_{taskToken: _, input: _} :< R1),
    atom_json_dict(R1.input, Input, []),
    sleep(1),

    aws_api_step_functions:send_task_heartbeat(
        'arn:aws:states:us-east-2:410388484666:activity:test',
        [status_code(Code2)], R1.taskToken, R2),
    assertion(R2 = _{}),
    sleep(1),

    format(string(Output), '"Hello, ~w!"', [Input.name]),
    aws_api_step_functions:send_task_success(
        'arn:aws:states:us-east-2:410388484666:activity:test',
        [status_code(Code3)], _{output: Output, taskToken: R1.taskToken}, R3),
    assertion(R3 = _{}).

test(failure, (Code0, Code1, Code2, Code3)  = (200, 200, 200, 200)) :-
    aws_api_step_functions:start_execution(
        'arn:aws:states:us-east-2:410388484666:stateMachine:Activitytask',
        [status_code(Code0)], _{name: "Activity"}, R0),
    assertion(_{executionArn: _,  startDate: _} :< R0),
    sleep(1),

    aws_api_step_functions:get_activity_task(
        'arn:aws:states:us-east-2:410388484666:activity:test',
        [status_code(Code1)], R1),
    assertion(_{taskToken: _, input: _} :< R1),
    sleep(1),

    aws_api_step_functions:send_task_heartbeat(
        'arn:aws:states:us-east-2:410388484666:activity:test',
        [status_code(Code2)], R1.taskToken, R2),
    assertion(R2 = _{}),
    sleep(1),

    aws_api_step_functions:send_task_failure(
        'arn:aws:states:us-east-2:410388484666:activity:test',
        [status_code(Code3)], _{cause: "send task failure api is called",
                                error: "activity task unit test",
                                taskToken: R1.taskToken}, R3),
    assertion(R3 = _{}).

:- end_tests(activity_task).
