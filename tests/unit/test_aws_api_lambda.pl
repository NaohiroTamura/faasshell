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

:- include('../../src/aws_api_lambda.pl').

%%
%% Unit Tests
%%
:- use_module(library(plunit)).

personal(Region, Account) :-
    getenv('aws_region', Region),
    getenv('aws_account_id', Account).

:- begin_tests(list).

test(hello, (Code, Name) = (200, "hello")) :-
    personal(Region, Account),
    atomic_list_concat([arn, aws, lambda, Region, Account, function, hello],
                       ':', ARN),
    aws_api_lambda:faas:list(ARN, [status_code(Code)], R),
    Name = R.'Configuration'.'FunctionName'.

:- end_tests(list).

:- begin_tests(frn_list).

test(hello, (Code, Name) = (200, "hello")) :-
    personal(Region, Account),
    atomic_list_concat([frn, aws, lambda, Region, Account, function, hello],
                       ':', ARN),
    aws_api_lambda:faas:list(ARN, [status_code(Code)], R),
    Name = R.'Configuration'.'FunctionName'.

:- end_tests(frn_list).

:- begin_tests(invoke).

test(hello_noarg, (Code, R) = (200, _{payload:"Hello, World!"})) :-
    personal(Region, Account),
    atomic_list_concat([arn, aws, lambda, Region, Account, function, hello],
                       ':', ARN),
    aws_api_lambda:faas:invoke(ARN, [status_code(Code)], _{}, R).

test(hello_arg, (Code, R) = (200, _{payload:"Hello, lambda!"})) :-
    personal(Region, Account),
    atomic_list_concat([arn, aws, lambda, Region, Account, function, hello],
                       ':', ARN),
    aws_api_lambda:faas:invoke(ARN, [status_code(Code)], _{name:"lambda"}, R).

test(hello_badarg, (Code, R) = (200, _{payload:"Hello, World!"})) :-
    personal(Region, Account),
    atomic_list_concat([arn, aws, lambda, Region, Account, function, hello],
                       ':', ARN),
    aws_api_lambda:faas:invoke(ARN, [status_code(Code)], '', R).

:- end_tests(invoke).

:- begin_tests(frn_invoke).

test(hello_arg, (Code, R) = (200, _{payload:"Hello, lambda!"})) :-
    personal(Region, Account),
    atomic_list_concat([frn, aws, lambda, Region, Account, function, hello],
                       ':', ARN),
    aws_api_lambda:faas:invoke(ARN, [status_code(Code)], _{name:"lambda"}, R).

:- end_tests(frn_invoke).

:- begin_tests(delete).

test(unknown, (Code, Message) = (404, 'Function not found')) :-
    aws_api_lambda:delete('arn:aws:lambda:us-east-2:000000000000:function:unknown',
                          [status_code(Code)], R),
    atomic_list_concat([Message |_], ':', R.'Message').

:- end_tests(delete).

:- begin_tests(custom_error).

test(nodejs, Code = 200) :-
    personal(Region, Account),
    atomic_list_concat([arn, aws, lambda, Region, Account, function, error],
                       ':', ARN),
    aws_api_lambda:faas:invoke(ARN, [status_code(Code)], _{}, R),

    assertion(R = _{error: "CustomError",
                    cause: _{errorMessage: "This is a custom error!",
                             errorType: "CustomError",
                             stackTrace: _ }}).

test(nodejs_dynamic, Code = 200) :-
    personal(Region, Account),
    atomic_list_concat([arn, aws, lambda, Region, Account, function, error],
                       ':', ARN),
    aws_api_lambda:faas:invoke(ARN, [status_code(Code)],
                               _{error: "new Error('Created dynamically!')"}, R),

    assertion(R = _{error: "Error",
                    cause: _{errorMessage: "Created dynamically!",
                             errorType: "Error",
                             stackTrace: _ }}).

test(python, Code = 200) :-
    personal(Region, Account),
    atomic_list_concat([arn, aws, lambda, Region, Account, function, raise],
                       ':', ARN),
    aws_api_lambda:faas:invoke(ARN, [status_code(Code)], _{}, R),

    assertion(R = _{error: "CustomError",
                    cause: _{errorMessage: "This is a custom error!",
                             errorType: "CustomError",
                             stackTrace: _ }}).

test(python_dynamic, Code = 200) :-
    personal(Region, Account),
    atomic_list_concat([arn, aws, lambda, Region, Account, function, raise],
                       ':', ARN),
    aws_api_lambda:faas:invoke(ARN, [status_code(Code)],
                               _{error: "AssertionError('Created dynamically!')"}, R),

    assertion(R = _{error: "AssertionError",
                    cause: _{errorMessage: "Created dynamically!",
                             errorType: "AssertionError",
                             stackTrace: _ }}).

:- end_tests(custom_error).
