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

test(auth_error, Code = 401) :-
    api_host(Host),
    string_concat(Host, '/statemachine/', URL),
    http_get(URL, Data, [authorization(basic(unknown, ng)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{error: "Authentication Failure"} = Dict).

test(put_create, Code = 200) :-
    load_json('samples/asl/hello_world_task_asl.json', Term),
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task_asl.json', URL),
    http_put(URL, json(Term), Data,
             [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{output: "ok", name: "hello_world_task_asl.json",
                namespace: "guest", dsl: _, asl: _} = Dict).

test(put_overwrite_true, Code = 200) :-
    load_json('samples/asl/hello_world_task_asl.json', Term),
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task_asl.json?overwrite=true',
                  URL),
    http_put(URL, json(Term), Data,
             [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{output: "ok", name: "hello_world_task_asl.json",
                namespace: "guest", dsl: _, asl: _} = Dict).

test(put_overwrite_false, Code = 409) :-
    load_json('samples/asl/hello_world_task_asl.json', Term),
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task_asl.json?overwrite=false',
                  URL),
    http_put(URL, json(Term), Data,
             [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{error:"conflict", reason:"Document update conflict."} :< Dict).

test(get, Code = 200) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task_asl.json', URL),
    http_get(URL, Data, [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{output: "ok", name: "hello_world_task_asl.json",
                namespace: "guest", dsl: _, asl: _} :< Dict).

test(get_view, Code = 200) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/', URL),
    http_get(URL, Data, [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{output: "ok", asl: [_|_]} :< Dict).

test(get_not_exit, Code = 404) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/not_exist.json', URL),
    http_get(URL, Data, [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{error:"not_found", reason:"missing"} :< Dict).

test(post, Code = 200) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task_asl.json', URL),
    term_json_dict(Json, _{input: _{name: "Statemachine"}}),
    http_post(URL, json(Json), Data,
              [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{asl: _, dsl: _, input: _{name:"Statemachine"}, name: _,
                namespace: _, output: _{payload:"Hello, Statemachine!"}} :< Dict).

test(post_empty_input, Code = 200) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task_asl.json', URL),
    term_json_dict(Json, _{input: _{}}),
    http_post(URL, json(Json), Data,
              [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{asl: _, dsl: _, input: _{}, name: _, namespace: _,
                output: _{payload:"Hello, World!"}} :< Dict).

test(post_no_input, Code = 400) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task_asl.json', URL),
    term_json_dict(Json, _{}),
    http_post(URL, json(Json), Data,
              [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{error: "Missing input key in params"} = Dict).

test(patch, Code = 200) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task_asl.json', URL),
    http_patch(URL, atom(''), Data,
               [authorization(basic(ID, PW)), status_code(Code)]),
    split_string(Data, "\n", "", [Output|_]),
    assertion(Output = "digraph graph_name {").

test(delete, Code = 200) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task_asl.json', URL),
    http_delete(URL, Data, [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{output: "ok"} :< Dict).

test(delete_not_exist, Code = 404) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/not_exist.json', URL),
    http_delete(URL, Data, [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{error:"not_found", reason:"missing"} :< Dict).

:- end_tests(hello_world_task).

%%
:- begin_tests(job_status_poller).

test(scenario) :-
    %% 1. setup
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/job_status_poller_asl.json', URL),
    string_concat(URL, '?overwrite=true', URL1),
    load_json('samples/asl/job_status_poller_asl.json', Term),
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


%%
:- begin_tests(has_dupes_asl).

test(put_create, Code = 500) :-
    load_json('samples/asl/has-dupes_asl.json', Term),
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/has-dupes_asl.json', URL),
    http_put(URL, json(Term), Data,
             [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{error:"syntax error", reason: _} = Dict).

:- end_tests(has_dupes_asl).

%%
:- begin_tests(choice_state).

test(put_overwrite_true, Code = 200) :-
    load_json('samples/asl/choice_state_asl.json', Term),
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/choice_state_asl.json?overwrite=true',
                  URL),
    http_put(URL, json(Term), Data,
             [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{output: "ok", name: "choice_state_asl.json",
                namespace: "guest", dsl: _, asl: _} = Dict).

test(first_state, Code = 200) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/choice_state_asl.json', URL),
    term_json_dict(Json, _{input: _{foo: 1}}),
    http_post(URL, json(Json), Data,
              [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{asl: _, dsl: _, input: _{foo:1}, name: _, namespace: _,
                output: _{foo: 1, first_match_state: _, next_state: _}} :< Dict).

test(second_state, Code = 200) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/choice_state_asl.json', URL),
    term_json_dict(Json, _{input: _{foo: 2}}),
    http_post(URL, json(Json), Data,
              [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{asl: _, dsl: _, input: _{foo:2}, name: _, namespace: _,
                output: _{foo: 2, second_match_state: _, next_state: _}} :< Dict).

test(default_state, Code = 200) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/choice_state_asl.json', URL),
    term_json_dict(Json, _{input: _{foo: 3}}),
    http_post(URL, json(Json), Data,
              [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{asl: _, dsl: _, input: _{foo:3}, name: _, namespace: _,
                output: _{cause:"No Matches!",error:"DefaultStateError"}} :< Dict).

:- end_tests(choice_state).

%%
:- begin_tests(choicex_state).

test(put_overwrite_true, Code = 200) :-
    load_json('samples/asl/choicex_state_asl.json', Term),
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/choicex_state_asl.json?overwrite=true',
                  URL),
    http_put(URL, json(Term), Data,
             [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{output: "ok", name: "choicex_state_asl.json",
                namespace: "guest", dsl: _, asl: _} = Dict).

test(public_state, Code = 200) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/choicex_state_asl.json', URL),
    term_json_dict(Json, _{input: _{type: "Public"}}),
    http_post(URL, json(Json), Data,
              [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{asl: _, dsl: _, input: _{type: "Public"}, name: _, namespace: _,
                output: _{type: "Public", public_state: _, next_state: _}} :< Dict).

test(value_is_zero_state, Code = 200) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/choicex_state_asl.json', URL),
    term_json_dict(Json, _{input: _{type:"Private", value:0}}),
    http_post(URL, json(Json), Data,
              [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{asl: _, dsl: _, input: _{type:"Private", value:0},
                name: _, namespace: _,
                output: _{type:"Private", value:0,
                          value_is_zero_state: _, next_state: _}} :< Dict).

test(value_in_twenties_state, Code = 200) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/choicex_state_asl.json', URL),
    term_json_dict(Json, _{input: _{type:"Private", value:25}}),
    http_post(URL, json(Json), Data,
              [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{asl: _, dsl: _, input: _{type:"Private", value:25},
                name: _, namespace: _,
                output: _{type:"Private", value:25,
                          value_in_twenties_state: _, next_state: _}} :< Dict).

test(default_state, Code = 200) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/choicex_state_asl.json', URL),
    term_json_dict(Json, _{input: _{type:"Private", value:35}}),
    http_post(URL, json(Json), Data,
              [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{asl: _, dsl: _, input: _{type:"Private", value:35},
                name: _, namespace: _,
                output: _{cause:"No Matches!", error:null}} :< Dict).

:- end_tests(choicex_state).

%%
:- begin_tests(wait_state).

test(put_overwrite_true, Code = 200) :-
    load_json('samples/asl/wait_state_asl.json', Term),
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/wait_state_asl.json?overwrite=true',
                  URL),
    http_put(URL, json(Term), Data,
             [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{output: "ok", name: "wait_state_asl.json",
                namespace: "guest", dsl: _, asl: _} = Dict).

test(wait_state, Code = 200) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/wait_state_asl.json', URL),
    term_json_dict(Json, _{input: _{name: "Lambda",
                                    expirydate: "2017-09-04T01:59:00Z",
                                    expiryseconds: 1}}),
    http_post(URL, json(Json), Data,
              [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{asl: _, dsl: _,
                input: _{name: "Lambda",
                         expirydate: "2017-09-04T01:59:00Z",
                         expiryseconds: 1}, name: _, namespace: _,
                output: _{payload: "Hello, Lambda!"}} :< Dict).


:- end_tests(wait_state).

%%
:- begin_tests(parallel).

test(put_overwrite_true, Code = 200) :-
    load_json('samples/asl/parallel_asl.json', Term),
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/parallel_asl.json?overwrite=true',
                  URL),
    http_put(URL, json(Term), Data,
             [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{output: "ok", name: "parallel_asl.json",
                namespace: "guest", dsl: _, asl: _} = Dict).

test(wait_state, Code = 200) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/parallel_asl.json', URL),
    term_json_dict(Json, _{input: _{var:1}}),
    http_post(URL, json(Json), Data,
              [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{asl: _, dsl: _, input: _{var:1}, name: _, namespace: _,
                output: [_{var:1}, _{var:1}]} :< Dict).

:- end_tests(parallel).

%%
:- begin_tests(pass_state).

test(put_overwrite_true, Code = 200) :-
    load_json('samples/asl/hello_world_asl.json', Term),
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_asl.json?overwrite=true',
                  URL),
    http_put(URL, json(Term), Data,
             [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{output: "ok", name: "hello_world_asl.json",
                namespace: "guest", dsl: _, asl: _} = Dict).

test(hello_world, Code = 200) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_asl.json', URL),
    term_json_dict(Json, _{input: _{}}),
    http_post(URL, json(Json), Data,
              [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{asl: _, dsl: _, input: _{}, name: _, namespace: _,
                output: "Hello World!"} :< Dict).

:- end_tests(pass_state).

%%
:- begin_tests(catch_failure).

test(put_overwrite_true, Code = 200) :-
    load_json('samples/asl/catch_failure_asl.json', Term),
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/catch_failure_asl.json?overwrite=true',
                  URL),
    http_put(URL, json(Term), Data,
             [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{output: "ok", name: "catch_failure_asl.json",
                namespace: "guest", dsl: _, asl: _} = Dict).

test(custom_error, Code = 200) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/catch_failure_asl.json', URL),
    term_json_dict(Json, _{input: _{}}),
    http_post(URL, json(Json), Data,
              [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{asl: _, dsl: _, input: _{}, name: _, namespace: _,
                output: "This is a fallback from a custom lambda function exception"} :< Dict).

test(reserved_type, Code = 200) :-
    api_host(Host), api_key(ID-PW),
    string_concat(Host, '/statemachine/catch_failure_asl.json', URL),
    term_json_dict(Json, _{input: _{error: "new Error('Created dynamically!')"}}),
    http_post(URL, json(Json), Data,
              [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{asl: _, dsl: _,  name: _, namespace: _,
                input: _{error: "new Error('Created dynamically!')"},
                output: "This is a fallback from a reserved error code"} :< Dict).

:- end_tests(catch_failure).
