%% -*- mode: prolog; coding: utf-8; -*-
%%
%% Copyright 2018 FUJITSU LIMITED
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

:- include('../../src/k5_api_functions.pl').

%%
%% Unit Tests
%%
:- use_module(library(plunit)).


:- begin_tests(invoke, [condition((getenv('HTTPS_PROXY',HTTPS_PROXY),
                                   HTTPS_PROXY \== ''))]).

test(hello_noarg, (Code, R) = (200, _{payload:"Hello, World!"})) :-
    k5_api_functions:faas:invoke('frn:k5:functions:::function:hello.js',
                                   [status_code(Code)], _{}, R).

test(hello_arg, (Code, R) = (200, _{payload:"Hello, K5!"})) :-
    k5_api_functions:faas:invoke('frn:k5:functions:::function:hello.js',
                                   [status_code(Code)], _{name: "K5"}, R).

test(hello_badarg, (Code, R) = (200, _{payload:"Hello, World!"})) :-
    k5_api_functions:faas:invoke('frn:k5:functions:::function:hello.js',
                                   [status_code(Code)], '', R).

:- end_tests(invoke).
