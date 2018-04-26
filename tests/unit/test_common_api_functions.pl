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

:- use_module(src/common_api_functions).
:- use_module(src/aws_api_lambda).

%%
%% Unit Tests
%%
:- use_module(library(plunit)).


:- begin_tests(hello).

test(atom, Code = 200) :-
    common_api_functions:faas:invoke(
      'https://naohirotamura.github.io/faasshell/ns/faas#hello',
      [status_code(Code), faasshell_auth(demo)], _{name: "RDF"}, R),
    assertion(R = _{payload:"Hello, RDF!"}).

test(string, Code = 200) :-
    common_api_functions:faas:invoke(
      "https://naohirotamura.github.io/faasshell/ns/faas#hello",
      [status_code(Code), faasshell_auth(demo)], _{name: "RDF"}, R),
    assertion(R = _{payload:"Hello, RDF!"}).

:- end_tests(hello).
