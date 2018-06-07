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

:- module(aws_api_step_functions,
          [ create_statemachine/4,
            delete_statemachine/3,
            describe_statemachine/3,
            get_activity_task/3,
            send_task_heartbeat/4,
            send_task_success/4,
            send_task_failure/4,
            start_execution/4,
            stop_execution/4,
            describe_execution/4
         ]).

:- use_module(aws_api_utils).
:- use_module(json_utils).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).


create_statemachine(ARN, Options, Request, Reply) :-
    atomic_list_concat([arn, aws, states, _R, _ID, stateMachine, Name ], ':', ARN),
    Req1 = Request.put(_{name: Name}),
    atom_json_dict(Payload, Req1, []),
    aws_api_utils:aws_step_functions(
                          'CreateStateMachine', ARN, '', Payload, AwsOptions),
    merge_options(Options, AwsOptions, MergedOptions),
    option(url(URL), MergedOptions),
    http_post(URL, atom('application/x-amz-json-1.0', Payload), R1, MergedOptions),
    atom_json_dict(R1, Reply, []).

delete_statemachine(ARN, Options, Reply) :-
    format(string(Payload), '{"stateMachineArn": "~w"}', [ARN]),
    aws_api_utils:aws_step_functions(
                          'DeleteStateMachine', ARN, '', Payload, AwsOptions),
    merge_options(Options, AwsOptions, MergedOptions),
    option(url(URL), MergedOptions),
    http_post(URL, atom('application/x-amz-json-1.0', Payload), R1, MergedOptions),
    atom_json_dict(R1, Reply, []).

describe_statemachine(ARN, Options, Reply) :-
    format(string(Payload), '{"stateMachineArn": "~w"}', [ARN]),
    aws_api_utils:aws_step_functions(
                          'DescribeStateMachine', ARN, '', Payload, AwsOptions),
    merge_options(Options, AwsOptions, MergedOptions),
    option(url(URL), MergedOptions),
    http_post(URL, atom('application/x-amz-json-1.0', Payload), R1, MergedOptions),
    atom_json_dict(R1, Reply, []).

get_activity_task(ARN, Options, Reply) :-
    atomic_list_concat([arn, aws, states, _R, _ID, activity, Name ], ':', ARN),
    format(string(Payload),
           '{"activityArn": "~w", "workerName": "~w"}', [ARN, Name]),
    aws_api_utils:aws_step_functions(
                          'GetActivityTask', ARN, '', Payload, AwsOptions),
    merge_options(Options, AwsOptions, MergedOptions),
    option(url(URL), MergedOptions),
    http_post(URL, atom('application/x-amz-json-1.0', Payload), R1, MergedOptions),
    atom_json_dict(R1, Reply, []).

send_task_heartbeat(ARN, Options, Token, Reply) :-
    format(string(Payload), '{"taskToken": "~w"}', [Token]),
    aws_api_utils:aws_step_functions(
                          'SendTaskHeartbeat', ARN, '', Payload, AwsOptions),
    merge_options(Options, AwsOptions, MergedOptions),
    option(url(URL), MergedOptions),
    http_post(URL, atom('application/x-amz-json-1.0', Payload), R1, MergedOptions),
    atom_json_dict(R1, Reply, []).

send_task_success(ARN, Options, Request, Reply) :-
    atom_json_dict(Payload, Request, []),
    aws_api_utils:aws_step_functions(
                          'SendTaskSuccess', ARN, '', Payload, AwsOptions),
    merge_options(Options, AwsOptions, MergedOptions),
    option(url(URL), MergedOptions),
    http_post(URL, atom('application/x-amz-json-1.0', Payload), R1, MergedOptions),
    atom_json_dict(R1, Reply, []).

send_task_failure(ARN, Options, Request, Reply) :-
    atom_json_dict(Payload, Request, []),
    aws_api_utils:aws_step_functions(
                          'SendTaskFailure', ARN, '', Payload, AwsOptions),
    merge_options(Options, AwsOptions, MergedOptions),
    option(url(URL), MergedOptions),
    http_post(URL, atom('application/x-amz-json-1.0', Payload), R1, MergedOptions),
    atom_json_dict(R1, Reply, []).

start_execution(ARN, Options, Input, Reply) :-
    atom_json_dict(InputAtom, Input, []),
    atom_string(InputAtom, InputString),
    format(string(Payload), '{"stateMachineArn": "~w", "input": ~p}',
           [ARN, InputString]),
    aws_api_utils:aws_step_functions(
                          'StartExecution', ARN, '', Payload, AwsOptions),
    merge_options(Options, AwsOptions, MergedOptions),
    option(url(URL), MergedOptions),
    http_post(URL, atom('application/x-amz-json-1.0', Payload), R1, MergedOptions),
    atom_json_dict(R1, Reply, []).

stop_execution(ARN, Options, Request, Reply) :-
    atom_json_dict(Payload, Request, []),
    aws_api_utils:aws_step_functions(
                          'StopExecution', ARN, '', Payload, AwsOptions),
    merge_options(Options, AwsOptions, MergedOptions),
    option(url(URL), MergedOptions),
    http_post(URL, atom('application/x-amz-json-1.0', Payload), R1, MergedOptions),
    atom_json_dict(R1, Reply, []).

describe_execution(ARN, Options, Request, Reply) :-
    atom_json_dict(Payload, Request, []),
    aws_api_utils:aws_step_functions(
                          'DescribeExecution', ARN, '', Payload, AwsOptions),
    merge_options(Options, AwsOptions, MergedOptions),
    option(url(URL), MergedOptions),
    http_post(URL, atom('application/x-amz-json-1.0', Payload), R1, MergedOptions),
    atom_json_dict(R1, Reply, []).
