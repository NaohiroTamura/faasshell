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

:- include('../../src/ifttt_api_webhooks.pl').

%%
%% Unit Tests
%%
:- use_module(library(plunit)).


:- begin_tests(webhook).

test(ok, Code = 200) :-
    ifttt_api_webhooks:faas:invoke('frn:ifttt:webhooks:::function:gss',
                                   [status_code(Code)], _{value1: "hello"}, R),
    assertion(R = "Congratulations! You've fired the gss event").

test(wrong_key, Code = 401) :-
    getenv('IFTTT_KEY', BACKUP),
    setenv('IFTTT_KEY', 'fake'),
    ifttt_api_webhooks:faas:invoke('frn:ifttt:webhooks:::function:gss',
                                   [status_code(Code)], _{value1: "hello"}, R),
    assertion(R = _{errors:[_{message:"You sent an invalid key."}]}),
    setenv('IFTTT_KEY', BACKUP).

test(empty_key, Code = 404) :-
    getenv('IFTTT_KEY', BACKUP),
    unsetenv('IFTTT_KEY'),
    ifttt_api_webhooks:faas:invoke('frn:ifttt:webhooks:::function:gss',
                                   [status_code(Code)], _{value1: "hello"}, R),
    assertion(R = "Cannot POST /trigger/faasshell/with/key/\n"),
    setenv('IFTTT_KEY', BACKUP).

:- end_tests(webhook).
