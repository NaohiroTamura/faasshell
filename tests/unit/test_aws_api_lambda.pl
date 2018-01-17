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

:- begin_tests(list).

test(hello, (Code, Name) = (200, "hello")) :-
    aws_api_lambda:list('arn:aws:lambda:us-east-2:410388484666:function:hello',
                        [status_code(Code)], R),
    Name = R.'Configuration'.'FunctionName'.

:- end_tests(list).

:- begin_tests(invoke).

test(hello, (Code, R) = (200, _{payload:"Hello, lambda!"})) :-
    aws_api_lambda:faas:invoke('arn:aws:lambda:us-east-2:410388484666:function:hello',
                          [status_code(Code)], _{name:"lambda"}, R).

:- end_tests(invoke).

:- begin_tests(delete).

test(unknown, (Code, Message) = (404, 'Function not found')) :-
    aws_api_lambda:delete('arn:aws:lambda:us-east-2:000000000000:function:unknown',
                          [status_code(Code)], R),
    atomic_list_concat([Message |_], ':', R.'Message').

:- end_tests(delete).
