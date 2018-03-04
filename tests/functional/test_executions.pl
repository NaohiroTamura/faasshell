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
:- begin_tests(executions, []).

test(background) :-
    load_json('samples/wsk/asl/hello_world_task.json', Term),
    faasshell_api_host(Host), faasshell_api_key(ID-PW),
    string_concat(Host, '/statemachine/hello_world_task.json', URL),
    string_concat(URL, '?overwrite=true', URL1),
    http_put(URL1, json(Term), _Data1,
             [authorization(basic(ID, PW)), status_code(Code1)]),
    assertion(Code1 = 200),

    string_concat(URL, '?blocking=false', URL2),
    term_json_dict(Json2, _{input: _{name: "Background"}}),
    http_post(URL2, json(Json2), Data2,
              [authorization(basic(ID, PW)), status_code(Code2)]),
    term_json_dict(Data2, Dict2),
    assertion(Code2 = 200),
    assertion(_{output: _{execution_id: _}} :< Dict2),

    sleep(5), %% needs to wait for background job completion

    atomics_to_string([Host, '/executions/', Dict2.output.execution_id], URL3),
    http_get(URL3, Data3, [authorization(basic(ID, PW)), status_code(Code3)]),
    term_json_dict(Data3, Dict3),
    assertion(Code3 = 200),
    assertion(
      _{ end: _,
         execution_id: _,
         hostname: _,
         namespace: "demo",
         result: _{ input: _{name: "Background"},
                    output: _{payload: "Hello, Background!"}
                  },
         start: _,
         statemachine: "hello_world_task.json"
       }  :< Dict3).

test(executionlist, Code = 200) :-
    faasshell_api_host(Host), faasshell_api_key(ID-PW),
    atomics_to_string([Host, '/executions/'], URL),
    http_get(URL, Data, [authorization(basic(ID, PW)), status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(is_list(Dict)),
    length(Dict, Len),
    assertion( Len >= 1).

:- end_tests(executions).
