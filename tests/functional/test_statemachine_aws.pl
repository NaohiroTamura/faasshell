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
%% Functional Tests for AWS Lambda
%%
:- begin_tests(hello_world_task).

test(put_overwrite_true, Code = 200) :-
    load_json('samples/aws/asl/hello_world_task.json', Term),
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task.json?overwrite=true',
                  URL),
    http_put(URL, json(Term), Data,
             [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{output: "ok", name: "hello_world_task.json",
                namespace: "guest", dsl: _, asl: _} = Dict).

test(get, Code = 200) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task.json', URL),
    http_get(URL, Data, [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{output: "ok", name: "hello_world_task.json",
                namespace: "guest", dsl: _, asl: _} :< Dict).

test(post, Code = 200) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task.json', URL),
    term_json_dict(Json, _{input: _{name: "Statemachine"}}),
    http_post(URL, json(Json), Data,
              [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{asl: _, dsl: _, input: _{name:"Statemachine"}, name: _,
                namespace: _, output: _{payload:"Hello, Statemachine!"}} :< Dict).

test(post_empty_input, Code = 200) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task.json', URL),
    term_json_dict(Json, _{input: _{}}),
    http_post(URL, json(Json), Data,
              [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{asl: _, dsl: _, input: _{}, name: _, namespace: _,
                output: _{payload:"Hello, World!"}} :< Dict).

test(post_no_input, Code = 400) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task.json', URL),
    term_json_dict(Json, _{}),
    http_post(URL, json(Json), Data,
              [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{error: "Missing input key in params"} = Dict).

test(delete, Code = 200) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task.json', URL),
    http_delete(URL, Data, [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{output: "ok"} :< Dict).

:- end_tests(hello_world_task).

%%
:- begin_tests(job_status_poller).

test(scenario) :-
    %% 1. setup
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/job_status_poller.json', URL),
    string_concat(URL, '?overwrite=true', URL1),
    load_json('samples/aws/asl/job_status_poller.json', Term),
    http_put(URL1, json(Term), _Data1,
             [authorization(basic(ID, PW)), status_code(Code1)]),
    assertion(Code1 = 200),

    %% 2. SUCCEEDED path
    term_json_dict(Json2, _{input: _{params: ['DEFAULT', 'SUCCEEDED'],
                                     wait_time: 1, name: "Poller"}}),
    http_post(URL, json(Json2), Data2,
              [authorization(basic(ID, PW)), status_code(Code2)]),
    term_json_dict(Data2, Dict2),
    assertion(Code2 = 200),
    assertion(Dict2.output = _{payload:"Hello, Poller!"}),

    %% 3. FAILED path
    term_json_dict(Json3, _{input: _{params: ['DEFAULT', 'FAILED'],
                                     wait_time: 1, name: "Poller"}}),
    http_post(URL, json(Json3), Data3,
              [authorization(basic(ID, PW)), status_code(Code3)]),
    term_json_dict(Data3, Dict3),
    assertion(Code3 = 200),
    assertion(Dict3.output = _{cause:"AWS Batch Job Failed",
                               error:"DescribeJob returned FAILED"}),

    %% 4. tear down
    http_delete(URL, _Data4, [authorization(basic(ID, PW)), status_code(Code4)]),
    assertion(Code4 = 200).

:- end_tests(job_status_poller).
