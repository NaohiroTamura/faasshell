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

:- include('unit_test_utils.pl').

%%
%% Unit Tests
%%
:- use_module(library(plunit)).

:- begin_tests(list).

test(ns_hello, Name = "echo") :-
    wsk_api_utils:openwhisk(Options),
    wsk_api_actions:list('/whisk.system/utils/echo', Options, R),
    Name = R.name.

:- end_tests(list).

:- begin_tests(invoke).

test(echo, R = _{foo:1}) :-
    wsk_api_utils:openwhisk(Options),
    wsk_api_actions:faas:invoke("wsk:/whisk.system/utils/echo", Options,
                                _{foo:1}, R).

:- end_tests(invoke).

:- begin_tests(all_actions, [setup(remove_hello_action)]).

test(scenarios) :-
    wsk_api_utils:openwhisk(Options),
    setup_call_cleanup(
            open('samples/actions/hello.js', read, S),
            read_string(S, _N, Code),
            close(S)),
    Payload = _{ namespace: "_",
                 name: "hello",
                 exec: _{ kind: "nodejs:6",
                          code: Code
                        }
               },

    %% 1. create_hello
    wsk_api_actions:create(hello, Options, Payload, R1),
    _{name: Name1, version: Version1} :< R1,
    assertion((Name1, Version1) = ("hello", "0.0.1")),

    %% 2. list_hello
    wsk_api_actions:list(hello, Options, R2),
    _{name: Name2, version: Version2} :< R2,
    assertion((Name2, Version2) = ("hello", "0.0.1")),

    %% 3. update_hello
    wsk_api_actions:update(hello, Options, Payload, R3),
    _{name: Name3, version: Version3} :< R3,
    assertion((Name3, Version3) = ("hello", "0.0.2")),

    %% 4. list_updated_hello
    wsk_api_actions:list(hello, Options, R4),
    _{name: Name4, version: Version4} :< R4,
    assertion((Name4, Version4) = ("hello", "0.0.2")),

    %% 5. invoke_default_hello_with_no_param
    wsk_api_actions:faas:invoke('wsk:hello', Options, _{}, R5),
    assertion(R5 = _{payload: "Hello, World!"}),

    %% 6. invoke_ns_hello_with_no_param
    wsk_api_actions:faas:invoke('wsk:/guest/hello', Options, _{}, R6),
    assertion(R6 = _{payload: "Hello, World!"}),

    %% 7. invoke_hello_param,
    wsk_api_actions:faas:invoke('wsk:hello', Options, _{name:"wsk"}, R7),
    assertion(R7 = _{payload:"Hello, wsk!"}),

    %% 8. delete_hello,
    wsk_api_actions:delete(hello, Options, R8),
    _{name: Name8, version: Version8} :< R8,
    assertion((Name8, Version8) = ("hello", "0.0.2")).

:- end_tests(all_actions).

%%
:- begin_tests(custom_error,
               [setup(create_action("error",
                                    'samples/actions/error.js', "nodejs:6", [])),
                setup(create_action("raise",
                                    'samples/actions/raise.py', "python:2", [])),
                setup(create_action("exception",
                                    'samples/actions/exception.pl', "blackbox",
                                    [image("nao16t/swipl7action")]))
               ]).

test(nodejs, Code = 502) :-
    wsk_api_utils:openwhisk(Options),
    wsk_api_actions:faas:invoke('wsk:error', [status_code(Code)|Options], _{}, R),
    assertion(R = _{cause:"This is a custom error!",error:"CustomError"}).

test(nodejs_dynamic, Code = 502) :-
    wsk_api_utils:openwhisk(Options),
    wsk_api_actions:faas:invoke('wsk:error', [status_code(Code)|Options],
                                _{error: "new Error('Created dynamically!')"}, R),
    assertion(R = _{cause:"Created dynamically!",error:"Error"}).

test(python, Code = 502) :-
    wsk_api_utils:openwhisk(Options),
    wsk_api_actions:faas:invoke('wsk:raise', [status_code(Code)|Options], _{}, R),
    %% OpenWhisk Python Runtime Issue
    %% assertion(R = _{error:"An error has occurred: CustomError: This is a custom error!"}).
    assertion(R = _{error:"The action did not return a dictionary."}).

test(python_dynamic, Code = 502) :-
    wsk_api_utils:openwhisk(Options),
    wsk_api_actions:faas:invoke('wsk:raise', [status_code(Code)|Options],
                                _{error: "AssertionError('Created dynamically!')"}, R),
    %% OpenWhisk Python Runtime Issue
    %% assertion(R = _{error:"An error has occurred: AssertionError: Created dynamically!"}).
    assertion(R = _{error:"The action did not return a dictionary."}).

/* requires openwhisk-runtime-prolog which implemented AWS Lambda compatible
   CustomError handling */
test(prolog, Code = 200) :-
    wsk_api_utils:openwhisk(Options),
    wsk_api_actions:faas:invoke('wsk:exception',
                                [status_code(Code)|Options], _{}, R),
    assertion(R = _{errorMessage: "This is a custom error!",
                    errorType: "CustomError"}).

test(prolog_dynamic, Code = 200) :-
    wsk_api_utils:openwhisk(Options),
    wsk_api_actions:faas:invoke(
        'wsk:exception', [status_code(Code)|Options],
        _{error: "'MyCustomError'('This is my custom error!', 200)"}, R),
    assertion(R = _{errorMessage: "This is my custom error!",
                    errorType: "MyCustomError"}).

:- end_tests(custom_error).
