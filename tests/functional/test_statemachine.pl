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
:- begin_tests(hello_world_task).

test(put_default, Output = "ok") :-
    load_json('samples/asl/hello_world_task_asl.json', Term),
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task_asl.json', URL),
    http_put(URL, json(Term), Data, [authorization(basic(ID, PW))]),
    term_json_dict(Data, Dict),
    get_dict(output, Dict, Output).

test(put_overwrite_true, Output = "ok") :-
    load_json('samples/asl/hello_world_task_asl.json', Term),
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task_asl.json?overwrite=true',
                  URL),
    http_put(URL, json(Term), Data, [authorization(basic(ID, PW))]),
    term_json_dict(Data, Dict),
    get_dict(output, Dict, Output).

test(put_overwrite_false, (Output, Error) = ("ng", "conflict")) :-
    load_json('samples/asl/hello_world_task_asl.json', Term),
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task_asl.json?overwrite=false',
                  URL),
    http_put(URL, json(Term), Data, [authorization(basic(ID, PW))]),
    term_json_dict(Data, Dict),
    _{output:Output, error:Error} :< Dict.

test(get, Output = "ok") :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/', URL),
    http_get(URL, Data, [authorization(basic(ID, PW))]),
    term_json_dict(Data, Dict),
    get_dict(output, Dict, Output).

test(get_view, Output = "hello_world_task_asl.json") :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task_asl.json', URL),
    http_get(URL, Data, [authorization(basic(ID, PW))]),
    term_json_dict(Data, Dict),
    get_dict(name, Dict, Output).

test(get_not_exit, Output = _{output:"ng", error:"not_found", reason:_}) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/not_exist.json', URL),
    http_get(URL, Data, [authorization(basic(ID, PW))]),
    term_json_dict(Data, Output).

test(post_no_param, Output = "Hello, FaaS Shell!") :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task_asl.json', URL),
    http_post(URL, json(json([])), Data, [authorization(basic(ID, PW))]),
    term_json_dict(Data, Dict),
    get_dict(payload, Dict.output, Output).

test(post, Output = "Hello, Curl!") :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task_asl.json', URL),
    http_post(URL, json(json(['input'=json(['name'='Curl'])])), Data,
              [authorization(basic(ID, PW))]),
    term_json_dict(Data, Dict),
    get_dict(payload, Dict.output, Output).

test(patch, Output = "digraph graph_name {") :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task_asl.json', URL),
    http_patch(URL, atom(''), Data, [authorization(basic(ID, PW))]),
    split_string(Data, "\n", "", [Output|_]).

test(delete, Output = "ok") :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task_asl.json', URL),
    http_delete(URL, Data, [authorization(basic(ID, PW))]),
    term_json_dict(Data, Dict),
    get_dict(output, Dict, Output).

test(delete_not_exist, Output = _{output:"ng", error:"not_found", reason:_}) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/not_exist.json', URL),
    http_delete(URL, Data, [authorization(basic(ID, PW))]),
    term_json_dict(Data, Output).

:- end_tests(hello_world_task).

%%
:- begin_tests(job_status_poller).

test(scenario) :-
    %% 1.
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task_asl.json', URL),
    http_delete(URL, Data1, [authorization(basic(ID, PW))]),
    term_json_dict(Data1, Dict1),
    assertion((Dict1.output = "ok"; Dict1.error = "not_found")),

    %% 2.
    load_json('samples/asl/job_status_poller_asl.json', Term),
    http_put(URL, json(Term), Data2, [authorization(basic(ID, PW))]),
    term_json_dict(Data2, Dict2),
    assertion(Dict2.output = "ok"),

    %% 3.
    http_post(URL,
              json(json([input=json([params=['DEFAULT', 'SUCCEEDED'],
                                     wait_time=1])])),
              Data3, [authorization(basic(ID, PW))]),
    term_json_dict(Data3, Dict3),
    assertion(Dict3.output.status = "SUCCEEDED"),

    %% 4.
    http_post(URL,
              json(json([input=json([params=['DEFAULT', 'FAILED'],
                                     wait_time=1])])),
              Data4, [authorization(basic(ID, PW))]),
    term_json_dict(Data4, Dict4),
    assertion(Dict4.output.status = "FAILED").

:- end_tests(job_status_poller).
