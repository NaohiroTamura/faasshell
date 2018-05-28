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
    wsk_api_actions:faas:list('frn:wsk:functions:::function:/whisk.system/utils/echo', [], R),
    Name = R.name.

:- end_tests(list).

:- begin_tests(invoke).

test(echo, R = _{foo:1}) :-
    wsk_api_actions:faas:invoke("frn:wsk:functions:::function:/whisk.system/utils/echo", [], _{foo:1}, R).

:- end_tests(invoke).

:- begin_tests(all_actions, [setup(remove_action(hello))]).

test(scenarios) :-
    setup_call_cleanup(
            open('samples/wsk/actions/hello.js', read, S),
            read_string(S, _N, Code),
            close(S)),
    Payload = _{ namespace: "_",
                 name: "hello",
                 exec: _{ kind: "nodejs:6",
                          code: Code
                        }
               },

    %% 1. create_hello
    wsk_api_actions:create(hello, [], Payload, R1),
    _{name: Name1, version: Version1} :< R1,
    assertion((Name1, Version1) = ("hello", "0.0.1")),

    %% 2. list_hello
    wsk_api_actionions:faas:list('frn:wsk:functions:::function:hello', [], R2),
    _{name: Name2, version: Version2} :< R2,
    assertion((Name2, Version2) = ("hello", "0.0.1")),

    %% 3. update_hello
    wsk_api_actions:update(hello, [], Payload, R3),
    _{name: Name3, version: Version3} :< R3,
    assertion((Name3, Version3) = ("hello", "0.0.2")),

    %% 4. list_updated_hello
    wsk_api_actions:faas:list('frn:wsk:functions:::function:hello', [], R4),
    _{name: Name4, version: Version4} :< R4,
    assertion((Name4, Version4) = ("hello", "0.0.2")),

    %% 5. invoke_default_hello_with_no_param
    wsk_api_actions:faas:invoke('frn:wsk:functions:::function:hello', [], _{}, R5),
    assertion(R5 = _{payload: "Hello, World!"}),

    %% 6. invoke_ns_hello_with_no_param
    wsk_api_actions:faas:invoke('frn:wsk:functions:::function:/_/hello', [], _{}, R6),
    assertion(R6 = _{payload: "Hello, World!"}),

    %% 7. invoke_hello_param,
    wsk_api_actions:faas:invoke('frn:wsk:functions:::function:hello', [], _{name:"wsk"}, R7),
    assertion(R7 = _{payload:"Hello, wsk!"}),

    %% 8. delete_hello,
    wsk_api_actions:delete(hello, [], R8),
    _{name: Name8, version: Version8} :< R8,
    assertion((Name8, Version8) = ("hello", "0.0.2")).

:- end_tests(all_actions).

%%
:- begin_tests(custom_error,
               [setup(( update_action("error",
                                      'samples/wsk/actions/error.js', "nodejs:6", []),
                        update_action("raise",
                                      'samples/wsk/actions/raise.py', "python:2", []),
                        update_action("exception",
                                      'samples/wsk/actions/exception.pl', "blackbox",
                                      [image("nao16t/swipl7action")])))
               ]).

test(nodejs, Code = 502) :-
    catch(wsk_api_actions:faas:invoke('frn:wsk:functions:::function:error',
                                      [status_code(Code)], _{}, _R),
          Error,
          true),
    assertion(Error = _{cause:"This is a custom error!",error:"CustomError"}).

test(nodejs_dynamic, Code = 502) :-
    catch(wsk_api_actions:faas:invoke('frn:wsk:functions:::function:error',
                                      [status_code(Code)],
                                      _{error: "new Error('Created dynamically!')"},
                                      _R),
          Error,
          true),
    assertion(Error = _{cause:"Created dynamically!",error:"Error"}).

test(python, Code = 502) :-
    catch(wsk_api_actions:faas:invoke('frn:wsk:functions:::function:raise',
                                      [status_code(Code)], _{}, _R),
          Error,
          true),
    %% OpenWhisk Python Runtime Issue
    %% assertion(Error = _{error:"An error has occurred: CustomError: This is a custom error!"}).
    assertion(Error = _{error:"The action did not return a dictionary."}).

test(python_dynamic, Code = 502) :-
    catch(wsk_api_actions:faas:invoke('frn:wsk:functions:::function:raise',
                                      [status_code(Code)],
                                      _{error: "AssertionError('Created dynamically!')"},
                                      _R),
          Error,
          true),
    %% OpenWhisk Python Runtime Issue
    %% assertion(Error = _{error:"An error has occurred: AssertionError: Created dynamically!"}).
    assertion(Error = _{error:"The action did not return a dictionary."}).

/* requires openwhisk-runtime-prolog which implemented AWS Lambda compatible
   CustomError handling */
test(prolog, Code = 200) :-
    wsk_api_actions:faas:invoke('frn:wsk:functions:::function:exception',
                                [status_code(Code)], _{}, R),
    assertion(R = _{errorMessage: "This is a custom error!",
                    errorType: "CustomError"}).

test(prolog_dynamic, Code = 200) :-
    wsk_api_actions:faas:invoke(
        'frn:wsk:functions:::function:exception', [status_code(Code)],
        _{error: "'MyCustomError'('This is my custom error!', 200)"}, R),
    assertion(R = _{errorMessage: "This is my custom error!",
                    errorType: "MyCustomError"}).

:- end_tests(custom_error).
