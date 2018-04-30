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
:- begin_tests(event_state).

test(auth_error, Code = 401) :-
    faasshell_api_host(Host),
    Event = "frn::states:::event:test",
    atomics_to_string([Host, '/trigger/', Event], TriggerURL),
    Action = "frn:wsk:functions:::function:hello",
    term_json_dict(Term, _{action: Action}),
    http_post(TriggerURL, json(Term), Data,
              [authorization(basic(unknown, ng)), cert_verify_hook(cert_accept_any),
               status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{error: "Authentication Failure"} = Dict).

test(event_name_missing, Code = 400) :-
    faasshell_api_host(Host), faasshell_api_key(ID-PW),
    atomics_to_string([Host, '/trigger/'], TriggerURL),
    Action = "frn:wsk:functions:::function:hello",
    term_json_dict(Term, _{action: Action}),
    http_post(TriggerURL, json(Term), Data,
              [authorization(basic(ID, PW)), cert_verify_hook(cert_accept_any),
               status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{error: "Missing event name"} = Dict).

test(succeed, (Code1, Code2, Code3, Status) = (200, 200, 200, true)) :-
    faasshell_api_host(Host), faasshell_api_key(ID-PW),

    load_json('samples/common/asl/event_state.json', Term1),
    string_concat(Host, '/statemachine/event_state.json?overwrite=true',
                  URL1),
    http_put(URL1, json(Term1), Data1,
             [authorization(basic(ID, PW)), cert_verify_hook(cert_accept_any),
              status_code(Code1)]),
    term_json_dict(Data1, Dict1),
    assertion(_{output: "ok", name: "event_state.json",
                namespace: "demo", dsl: _, asl: _} = Dict1),

    string_concat(Host, '/statemachine/event_state.json?blocking=true', URL2),
    term_json_dict(Term2, _{input: _{name: "Event"}}),
    message_queue_create(MQueue),
    thread_create(
            ( http_post(URL2, json(Term2), Data2,
                        [authorization(basic(ID, PW)),
                         cert_verify_hook(cert_accept_any), status_code(Code2)]),
              thread_send_message(MQueue, test_result(Data2))
            ),
            Id),

    Event = "frn::states:::event:test",
    atomics_to_string([Host, '/trigger/', Event], TriggerURL),

    Action = "frn:wsk:functions:::function:hello",
    term_json_dict(Term3, _{action: Action}),
    http_post(TriggerURL, json(Term3), Data3,
               [authorization(basic(ID, PW)), cert_verify_hook(cert_accept_any),
                status_code(Code3)]),
    term_json_dict(Data3, Dict3),
    assertion(Dict3 = _{}),

    thread_join(Id, Status),
    thread_get_message(MQueue, test_result(Data4)),
    term_json_dict(Data4, Dict4),
    assertion(_{asl: _, dsl: _, input: _{name:"Event"}, name: _,
                namespace: _, output: _{payload:"Hello, Event!"}} :< Dict4).

test(server_timeout, (Code1, Code2, Status) = (200, 200, true)) :-
    faasshell_api_host(Host), faasshell_api_key(ID-PW),

    load_json('samples/common/asl/event_state_option.json', Term1),
    string_concat(Host, '/statemachine/event_state_option.json?overwrite=true',
                  URL1),
    http_put(URL1, json(Term1), Data1,
             [authorization(basic(ID, PW)), cert_verify_hook(cert_accept_any),
              status_code(Code1)]),
    term_json_dict(Data1, Dict1),
    assertion(_{output: "ok", name: "event_state_option.json",
                namespace: "demo", dsl: _, asl: _} = Dict1),

    string_concat(Host, '/statemachine/event_state_option.json?blocking=true', URL2),
    term_json_dict(Term2, _{input: _{name: "Event"}}),
    message_queue_create(MQueue),
    thread_create(
            ( http_post(URL2, json(Term2), Data2,
                        [authorization(basic(ID, PW)),
                         cert_verify_hook(cert_accept_any), status_code(Code2)]),
              thread_send_message(MQueue, test_result(Data2))
            ),
            Id),

    thread_join(Id, Status),
    thread_get_message(MQueue, test_result(Data3)),
    term_json_dict(Data3, Dict3),
    assertion(_{asl: _, dsl: _, input: _{name:"Event"}, name: _,
                namespace: _, output: _{error:"States.Timeout"}} :< Dict3).

test(client_timeout, Code = 400) :-
    faasshell_api_host(Host), faasshell_api_key(ID-PW),

    Event = "frn::states:::event:test",
    atomics_to_string([Host, '/trigger/', Event], TriggerURL),

    Action = "frn:wsk:functions:::function:hello",
    term_json_dict(Term, _{action: Action, timeout: 1}),
    http_post(TriggerURL, json(Term), Data,
               [authorization(basic(ID, PW)), cert_verify_hook(cert_accept_any),
                status_code(Code)]),
    term_json_dict(Data, Dict),
    assertion(_{error: "Timeout subscribe event"} = Dict).

:- end_tests(event_state).
