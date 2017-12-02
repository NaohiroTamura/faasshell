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

:- include('../../src/wsk_api_actions.pl').

%%
%% Unit Tests
%%
:- use_module(library(plunit)).

:- begin_tests(list).

test(json_term_to_dict, Name  == "openwhisk") :-
    term_json_dict(json([name=openwhisk]), Dict), Name = Dict.name.

test(json_dict_to_term, Term  == json([name=openwhisk])) :-
    term_json_dict(Term, json{name:"openwhisk"}).

test(action_list, true) :-
   wsk_api_utils:openwhisk(Options), wsk_api_actions:list(hello, Options, _R).

:- end_tests(list).

:- begin_tests(invoke).
%%
test(hello, R = _{payload:"Hello, wsk!"}) :-
    wsk_api_utils:openwhisk(Options),
    wsk_api_actions:invoke(hello,Options,_{name:"wsk"}, R).

test(echo, R = _{foo:1}) :-
    wsk_api_utils:openwhisk(Options),
    wsk_api_actions:invoke("/whisk.system/utils/echo",Options,_{foo:1}, R).

:- end_tests(invoke).
