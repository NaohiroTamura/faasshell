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
    api_host(Host),
    string_concat(Host, '/shell/', URL),
    http_get(URL, Data, [authorization(basic(unknown, ng)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{error: "Authentication Failure"} = Dict).

test(put_default, Output = "ok") :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/shell/hello_world_task.dsl', URL),
    http_put(URL, file('samples/dsl/hello_world_task.dsl'), Data,
             [authorization(basic(ID, PW))]),
    term_json_dict(Data, Dict),
    get_dict(output, Dict, Output).

test(put_overwrite_true, Output = "ok") :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/shell/hello_world_task.dsl?overwrite=true', URL),
    http_put(URL, file('samples/dsl/hello_world_task.dsl'), Data,
             [authorization(basic(ID, PW))]),
    term_json_dict(Data, Dict),
    get_dict(output, Dict, Output).

test(put_overwrite_false, (NG, Conflict) = ("ng", "conflict")) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/shell/hello_world_task.dsl?overwrite=false', URL),
    http_put(URL, file('samples/dsl/hello_world_task.dsl'), Data,
             [authorization(basic(ID, PW))]),
    term_json_dict(Data, Output),
    _{output:NG, error: Conflict, reason:_} :< Output.

test(get, Output = "ok") :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/shell/hello_world_task.dsl', URL),
    http_get(URL, Data, [authorization(basic(ID, PW))]),
    term_json_dict(Data, Dict),
    get_dict(output, Dict, Output).

test(get_view, Output = "ok") :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/shell/', URL),
    http_get(URL, Data, [authorization(basic(ID, PW))]),
    term_json_dict(Data, Dict),
    get_dict(output, Dict, Output).

test(get_not_exit, Output = _{output:"ng", error:"not_found", reason:_}) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/shell/not_exist.dsl', URL),
    http_get(URL, Data, [authorization(basic(ID, PW))]),
    term_json_dict(Data, Output).

test(post, Output = "Hello, FaaS Shell!") :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/shell/hello_world_task.dsl', URL),
    http_post(URL, json(json(['name'='FaaS Shell'])), Data,
              [authorization(basic(ID, PW))]),
    term_json_dict(Data, Dict),
    get_dict(payload, Dict.output, Output).

test(post_no_param, Output = "Hello, World!") :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/shell/hello_world_task.dsl', URL),
    http_post(URL, json(json([])), Data, [authorization(basic(ID, PW))]),
    term_json_dict(Data, Dict),
    get_dict(payload, Dict.output, Output).

test(delete, Output = "ok") :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/shell/hello_world_task.dsl', URL),
    http_delete(URL, Data, [authorization(basic(ID, PW))]),
    term_json_dict(Data, Dict),
    get_dict(output, Dict, Output).

test(delete_not_exist,Output =  _{output:"ng", error:"not_found", reason:_}) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/shell/not_exist.dsl', URL),
    http_delete(URL, Data, [authorization(basic(ID, PW))]),
    term_json_dict(Data, Output).

:- end_tests(hello_world_task_dsl).
