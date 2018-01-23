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

:- use_module(src/wsk_api_actions).
:- use_module(src/wsk_api_dcg).
:- use_module(src/wsk_api_utils).

%% setup utils

remove_hello_action :-
    wsk_api_utils:openwhisk(Options),
    catch( wsk_api_actions:delete(hello, Options, _R),
           _Error,
           (print_message(warning,
                          format("confirmed 'hello' doesn't exist.", [])),
            true)).

%%
create_action(Action, File, Kind, Container) :-
    wsk_api_utils:openwhisk(Options),
    open(File, read, S),
    call_cleanup(
            read_string(S, _N, Code),
            close(S)),
    Payload = _{ namespace: "_",
                 name: Action,
                 exec: _{ kind: Kind,
                          code: Code
                        }
               },
    ( option(image(Image), Container)
      -> PayloadOpt = Payload.exec.put(_{image: Image})
      ;  PayloadOpt = Payload
    ),
    catch( wsk_api_actions:create(Action, Options, PayloadOpt, _R),
           _Error,
           (print_message(warning,
                          format("confirmed '~w' exists.", [Action])),
            true)).

%%
functional_test_setup :-
    create_action("hello", 'samples/actions/hello.js', "nodejs:6", []),
    create_action("job", 'samples/actions/job.py', "python:2", []).
