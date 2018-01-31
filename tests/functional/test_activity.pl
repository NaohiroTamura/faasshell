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

:- use_module(library(plunit)).

:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).

:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).

:- include('functional_test_utils.pl').

%%
%% Functional Tests
%%
:- begin_tests(activity_task).

test(auth_error, Code = 401) :-
    api_host(Host),
    string_concat(Host, '/activity/', URL),
    http_get(URL, Data, [authorization(basic(unknown, ng)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{error: "Authentication Failure"} = Dict).

test(succeed, (Code1, Code2, Code3, Code4, Code5, Status)
              = (200, 200, 200, 200, 200, true)) :-
    api_host(Host), api_key(ID-PW),

    load_json('samples/asl/activity_task_asl.json', Term1),
    string_concat(Host, '/statemachine/activity_task_asl.json?overwrite=true',
                  URL1),
    http_put(URL1, json(Term1), Data1,
             [authorization(basic(ID, PW)), status_code(Code1)]),
    term_json_dict(Data1, Dict1),
    assertion(_{output: "ok", name: "activity_task_asl.json",
                namespace: "guest", dsl: _, asl: _} = Dict1),

    string_concat(Host, '/statemachine/activity_task_asl.json', URL2),
    term_json_dict(Term2, _{input: _{name: "Activity"}}),
    message_queue_create(MQueue),
    thread_create(
            ( http_post(URL2, json(Term2), Data2,
                        [authorization(basic(ID, PW)), status_code(Code2)]),
              thread_send_message(MQueue, test_result(Data2))
            ),
            Id),
    sleep(1),

    Activity = "arn:aws:states:us-east-2:410388484666:activity:test",
    atomics_to_string([Host, '/activity/', Activity], ActivityURL),

    http_get(ActivityURL, Data3, [authorization(basic(ID, PW)), status_code(Code3)]),
    term_json_dict(Data3, Dict3),
    assertion(Dict3 = _{output: "ok", taskToken: _,
                        input: _{name: "Activity"}}),
    sleep(1),

    term_json_dict(Term4, _{taskToken: Dict3.taskToken}),
    http_patch(ActivityURL, json(Term4), Data4,
               [authorization(basic(ID, PW)), status_code(Code4)]),
    term_json_dict(Data4, Dict4),
    assertion(Dict4 = _{}),
    sleep(1),

    atomics_to_string(["Hello, ", Dict3.input.name, "!"], Output),
    term_json_dict(Term5, _{output: _{payload: Output}, taskToken: Dict3.taskToken}),
    http_post(ActivityURL, json(Term5), Data5,
               [authorization(basic(ID, PW)), status_code(Code5)]),
    term_json_dict(Data5, Dict5),
    assertion(Dict5 = _{}),

    thread_join(Id, Status),
    thread_get_message(MQueue, test_result(Data2)),
    term_json_dict(Data2, Dict2),
    assertion(_{asl: _, dsl: _, input: _{name:"Activity"}, name: _,
                namespace: _, output: _{payload:"Hello, Activity!"}} :< Dict2).

:- end_tests(activity_task).
