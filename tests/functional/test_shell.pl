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
:- begin_tests(hello_world_task_dsl).

test(auth_error, Code = 401) :-
    faasshell_api_host(Host),
    string_concat(Host, '/shell/', URL),
    http_get(URL, Data, [authorization(basic(unknown, ng)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{error: "Authentication Failure"} = Dict).

test(put_create, Code = 200) :-
    faasshell_api_host(Host), faasshell_api_key(ID-PW),
    string_concat(Host, '/shell/hello_world_task.dsl', URL),
    http_put(URL, file('samples/dsl/hello_world_task.dsl'), Data,
             [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{output: "ok", name: "hello_world_task.dsl",
                namespace: "demo", dsl: _} = Dict).

test(put_overwrite_true, Code = 200) :-
    faasshell_api_host(Host), faasshell_api_key(ID-PW),
    string_concat(Host, '/shell/hello_world_task.dsl?overwrite=true', URL),
    http_put(URL, file('samples/dsl/hello_world_task.dsl'), Data,
             [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{output: "ok", name: "hello_world_task.dsl",
                namespace: "demo", dsl: _} :< Dict).

test(put_overwrite_false, Code = 409) :-
    faasshell_api_host(Host), faasshell_api_key(ID-PW),
    string_concat(Host, '/shell/hello_world_task.dsl?overwrite=false', URL),
    http_put(URL, file('samples/dsl/hello_world_task.dsl'), Data,
             [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{error:"conflict", reason:"Document update conflict."} :< Dict).

test(get, Code = 200) :-
    faasshell_api_host(Host), faasshell_api_key(ID-PW),
    string_concat(Host, '/shell/hello_world_task.dsl', URL),
    http_get(URL, Data, [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{output: "ok", name: "hello_world_task.dsl",
                namespace: "demo", dsl: _} :< Dict).

test(get_view, Code = 200) :-
    faasshell_api_host(Host), faasshell_api_key(ID-PW),
    string_concat(Host, '/shell/', URL),
    http_get(URL, Data, [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{output: "ok", dsl: [_|_]} :< Dict).

test(get_not_exit, Code = 404) :-
    faasshell_api_host(Host), faasshell_api_key(ID-PW),
    string_concat(Host, '/shell/not_exist.dsl', URL),
    http_get(URL, Data, [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{error:"not_found", reason:"missing"} :< Dict).

test(post, Code = 200) :-
    faasshell_api_host(Host), faasshell_api_key(ID-PW),
    term_json_dict(Json, _{input: _{name: "FaaS Shell"}}),
    string_concat(Host, '/shell/hello_world_task.dsl', URL),
    http_post(URL, json(Json), Data,
              [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{dsl: _, input: _{name:"FaaS Shell"}, name: _, namespace: _,
                output: _{payload:"Hello, FaaS Shell!"}} :< Dict).

test(post_empty_input, Code = 200) :-
    faasshell_api_host(Host), faasshell_api_key(ID-PW),
    string_concat(Host, '/shell/hello_world_task.dsl', URL),
    term_json_dict(Json, _{input: _{}}),
    http_post(URL, json(Json), Data,
              [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{dsl: _, input: _{}, name: _, namespace: _,
                output: _{payload:"Hello, World!"}} :< Dict).

test(post_no_input, Code = 400) :-
    faasshell_api_host(Host), faasshell_api_key(ID-PW),
    string_concat(Host, '/shell/hello_world_task.dsl', URL),
    term_json_dict(Json, _{}),
    http_post(URL, json(Json), Data,
              [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{error: "Missing input key in params"} :< Dict).

test(delete, Code = 200) :-
    faasshell_api_host(Host), faasshell_api_key(ID-PW),
    string_concat(Host, '/shell/hello_world_task.dsl', URL),
    http_delete(URL, Data, [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{output: "ok"} :< Dict).

test(delete_not_exist, Code = 404) :-
    faasshell_api_host(Host), faasshell_api_key(ID-PW),
    string_concat(Host, '/shell/not_exist.dsl', URL),
    http_delete(URL, Data, [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{error:"not_found", reason:"missing"} :< Dict).

:- end_tests(hello_world_task_dsl).

%%
:- begin_tests(hello_term_error_dsl).

test(put_create, Code = 500) :-
    faasshell_api_host(Host), faasshell_api_key(ID-PW),
    string_concat(Host, '/shell/hello_term_error.dsl', URL),
    http_put(URL, file('samples/dsl/hello_term_error.dsl'), Data,
             [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{error:"syntax error", reason: _} = Dict).

:- end_tests(hello_term_error_dsl).
