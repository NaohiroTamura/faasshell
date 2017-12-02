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

:- include('../../src/asl_gen.pl').

%%
%% Unit Tests
%%
:- use_module(library(plunit)).

:- begin_tests(blueprints).

test(hello) :- main('samples/blueprints/hello_world.json', _D, _G).

test(choice) :- main('samples/blueprints/choice_state.json', _D, _G).

test(choicex) :- main('samples/blueprints/choice_statex.json', _D, _G).

test(catch) :- main('samples/blueprints/catch_failure.json', _D, _G).

test(poller) :- main('samples/blueprints/job_status_poller.json', _D, _G).

test(parallel) :- main('samples/blueprints/parallel.json', _D, _G).

test(retry) :- main('samples/blueprints/retry_failure.json', _D, _G).

test(timer) :- main('samples/blueprints/task_timer.json', _D, _G).

test(wait) :- main('samples/blueprints/wait_state.json', _D, _G).

:- end_tests(blueprints).

:- begin_tests(invalid).

test(abnormal, error(duplicated_state(['Sub2_1']))) :-
    main('tests/unit/test_data/has-dupes.json', _D, _G).

test(abnormal, error(_)) :-
    main('tests/unit/test_data/linked-parallel.json', _D, _G).

test(abnormal) :-
    main('tests/unit/test_data/minimal-fail-state.json', _D, _G).

test(abnormal) :-
    main('tests/unit/test_data/no-terminal.json', _D, _G).

:- end_tests(invalid).
