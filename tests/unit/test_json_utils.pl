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

:- include('../../src/json_utils.pl').

%%
%% Unit Tests
%%
:- use_module(library(plunit)).

:- begin_tests(term_json_dict).

test(json_term_to_dict, Name  == "openwhisk") :-
    term_json_dict(json([name=openwhisk]), Dict), Name = Dict.name.

test(json_dict_to_term, Term  == json([name=openwhisk])) :-
    term_json_dict(Term, json{name:"openwhisk"}).

:- end_tests(term_json_dict).

:- begin_tests(json_path).

test(value, (K, R, V) = [a, b], _,  1) :-
    json_path_value('$.a.b', _{a:_{b:1}}, K, R, V).

test(list, (K, R, V) = [a, b], _, [0, 1, 2, 3, 4]) :-
    json_path_value('$.a.b', _{a:_{b:[0,1,2,3,4]}}, K, R, V).

test(range, (K, R, V) = [a, b], [0, 2], [0, 1]) :-
    json_path_value('$.a.b[0:2]', _{a:_{b:[0,1,2,3,4]}}, K, R, V).

test(underscore1, (K, R, V) = [a_b], _, 1) :-
    json_path_value('$.a_b', _{a_b:1}, K, R, V).

test(underscore2, (K, R, V) = [a_b_c], _, 1) :-
    json_path_value('$.a_b_c', _{a_b_c:1}, K, R, V).

test(underscore2, (K, R, V) = [a_b], [0, 2], [0, 1]) :-
    json_path_value('$.a_b[0:2]', _{a_b:[0,1,2,3,4]}, K, R, V).

test(key_error, error(existence_error(key, c, _{b:1}), _)) :-
    json_path_value('$.a.c', _{a:_{b:1}}, _K, _R, _V).

test(merge, (K, M) = ([b, c], _{a:1, b:_{c:2}})) :-
    json_path_merge('$.b.c', _{a:1}, 2, K, M).

test(dup1, (K, M) = ([a], _{a:2})) :-
    json_path_merge('$.a', _{a:1}, 2, K, M).

test(dup2, (K, M) = ([b, c], _{a:1, b:_{c:2}})) :-
    json_path_merge('$.b.c', _{a:1, b:3}, 2, K, M).

:- end_tests(json_path).
