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

:- include('../../src/azure_api_functions.pl').

%%
%% Unit Tests
%%
:- use_module(library(plunit)).


personal(WebAppName) :-
    getenv('azure_webapp_name', WebAppName).


:- begin_tests(list).

test(all, Code = 200) :-
    azure_api_functions:faas:list([], [status_code(Code)], R),
    assertion(is_list(R)).

test(hello, Code = 200) :-
    personal(WebAppName),
    atomic_list_concat([frn, azure, functions, '', WebAppName, function, hello],
                       ':', FRN),
    azure_api_functions:faas:list(FRN, [status_code(Code)], R),
    atomics_to_string([WebAppName, hello], '/', Name),
    assertion(Name = R.name).

:- end_tests(list).

:- begin_tests(invoke).

test(hello_noarg, (Code, R) = (200, _{payload:"Hello, World!"})) :-
    personal(WebAppName),
    atomic_list_concat([frn, azure, functions, '', WebAppName, function, hello],
                       ':', FRN),
    azure_api_functions:faas:invoke(FRN, [status_code(Code)], _{}, R).

test(hello_arg, (Code, R) = (200, _{payload:"Hello, Azure!"})) :-
    personal(WebAppName),
    atomic_list_concat([frn, azure, functions, '', WebAppName, function, hello],
                       ':', FRN),
    azure_api_functions:faas:invoke(FRN, [status_code(Code)], _{name: "Azure"}, R).

test(hello_badarg, (Code, R) = (200, _{payload:"Hello, World!"})) :-
    personal(WebAppName),
    atomic_list_concat([frn, azure, functions, '', WebAppName, function, hello],
                       ':', FRN),
    azure_api_functions:faas:invoke(FRN, [status_code(Code)], '', R).

:- end_tests(invoke).
