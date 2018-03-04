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

remove_action(Action) :-
    catch( wsk_api_actions:delete(Action, [], _R),
           _Error,
           (print_message(warning,
                          format("confirmed '~w' doesn't exist.", [Action])),
            true)).

%%
update_action(Action, File, Kind, Container) :-
    setup_call_cleanup(
            open(File, read, S),
            read_string(S, _N, Code),
            close(S)),
    Payload = _{ namespace: "_",
                 name: Action,
                 exec: _{ kind: Kind,
                          code: Code
                        },
                 limits: _{ timeout: 60000,
                            memory: 128,
                            logs: 5
                          }
               },
    ( option(image(Image), Container)
      -> PayloadOpt = Payload.put(exec/image, Image)
      ;  PayloadOpt = Payload
    ),
    wsk_api_actions:update(Action, [status_code(StatusCode)], PayloadOpt, R),
    ( StatusCode == 200
      -> true
      ;  print_message(error, format("failed update '~w' : ~w : ~w.",
                                     [Action, StatusCode, R]))
    ).

%%
faas_test_setup :-
    update_action("delay", 'samples/wsk/actions/delay.js', "nodejs:6", []),
    update_action("error", 'samples/wsk/actions/error.js', "nodejs:6", []),
    update_action("hello", 'samples/wsk/actions/hello.js', "nodejs:6", []),
    update_action("job", 'samples/wsk/actions/job.py', "python:2", []),
    update_action("raise", 'samples/wsk/actions/raise.py', "python:2", []),
    update_action("sleep", 'samples/wsk/actions/sleep.py', "python:2", []),
    update_action("sns", 'samples/wsk/actions/sns.py', "python:2", []),
    update_action("exception", 'samples/wsk/actions/exception.pl', "blackbox",
                  [image("nao16t/swipl7action")]).
