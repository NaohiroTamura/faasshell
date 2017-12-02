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

:- include('../../src/asl_run.pl').

%%
%% Unit Tests
%%
:- begin_tests(process_io).

test(input, Input = _{val1:3, val2:4}) :-
    OriginalInput = _{title: "Numbers to add", numbers: _{val1:3, val2:4}},
    Optional = [input_path(numbers)],
    process_input(OriginalInput, Input, Optional).

test(output, Output = 7) :-
    OriginalInput = _{title: "Numbers to add", numbers: _{val1:3, val2:4}},
    Optional = [input_path(numbers), result_path(sum), output_path(sum)],
    process_output(OriginalInput, 7, Output, Optional).

test(output, Output = _{a:1, parallel:[_{a:1},_{a:1}]}) :-
    OriginalInput = _{a:1},
    Optional = [result_path(parallel)],
    Result = [_{a:1}, _{a:1}],
    process_output(OriginalInput, Result, Output, Optional).

:- end_tests(process_io).

:- begin_tests(pass).

test(hello, O = _{message:"Hello World!", name:"wsk"}) :-
    start('samples/dsl/hello.dsl', _{name:"wsk"}, O).

:- end_tests(pass).

:- begin_tests(task).

test(hello, O = _{error:"States.Timeout",name:"wsk"}) :-
    start('samples/dsl/hello_task.dsl', _{name:"wsk"}, O).

:- end_tests(task).

:- begin_tests(choice).

test(default, O = _{cause:"No Matches!",error:"DefaultStateError",foo:5}) :-
    start('samples/dsl/choice_state.dsl', _{foo:5}, O).

:- end_tests(choice).
