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

:- include('../../src/wsk_api_dcg.pl').

%%
%% Unit Tests
%%
:- use_module(library(plunit)).

%% Actions Test
:- begin_tests(actions).

test(get, Xs == ["get", "/", "namespaces", "/", "guest", "/", "actions"]) :-
    phrase(path(get, guest, actions), Xs).

test(get_rev, (V, N, A) == (get, guest, actions)) :-
    phrase(path(V, N, A), ["get", "/", "namespaces", "/", "guest", "/", "actions"]).

test(illegal_verb, fail) :-
    phrase(path(put, guest, actions), _Xs).

test(action_name, Xs == ["get", "/", "namespaces", "/", "guest", "/", "actions",
                         "/", "hello"]) :-
    phrase(path(get, guest, actions, hello), Xs).

test(action_name_rev, (V, N, A, E) == (get, guest, actions, hello)) :-
    phrase(path(V, N, A, E),
           ["get", "/", "namespaces", "/", "guest", "/", "actions", "/", "hello"]).

test(query, Xs == ["get", "/", "namespaces", "/", "guest", "/", "actions",
                   "/", "hello", "?", "limit", "=", "1"]) :-
    path(get, guest, actions, hello, [limit=1], Xs, []).

test(query_rev, (V, N, A, E, Q)  == (get, guest, actions, hello, [limit=1])) :-
    path(V, N, A, E, Q, ["get", "/", "namespaces", "/", "guest", "/", "actions",
                        "/", "hello", "?", "limit", "=", "1"], []).

test(query_empty, Xs == ["get", "/", "namespaces", "/", "guest", "/", "actions",
                   "/", "hello"]) :-
    path(get, guest, actions, hello, [], Xs, []).


test(query, Xs == ["get", "/", "namespaces", "/", "guest", "/", "actions",
                   "/", "hello", "?", "skip", "=", "2"]) :-
    path(get, guest, actions, hello, [skip=2], Xs, []).

test(query, Xs == ["get", "/", "namespaces", "/", "guest", "/", "actions",
                   "/", "hello", "?", "limit", "=", "1", "&", "skip", "=", "2"]) :-
    path(get, guest, actions, hello, [limit=1, skip=2], Xs, []).

test(unknown_query, fail) :-
    path(get, guest, actions, hello, [unknown=true], _Xs, []).

test(put, Xs == ["put", "/", "namespaces", "/", "guest", "/", "actions",
                 "/", "hello"]) :-
    phrase(path(put, guest, actions, hello), Xs).

test(put, Xs == ["put", "/", "namespaces", "/", "guest", "/", "actions",
                 "/", "hello", "?", "overwrite", "=", "true"]) :-
    phrase(path(put, guest, actions, hello, [overwrite=true]), Xs).

test(delete, Xs == ["delete", "/", "namespaces", "/", "guest", "/", "actions",
                    "/", "hello"]) :-
    phrase(path(delete, guest, actions, hello), Xs).

test(post, Xs == ["post", "/", "namespaces", "/", "guest", "/", "actions",
                  "/", "hello"]) :-
    phrase(path(post, guest, actions, hello), Xs).

test(query, Xs == ["post", "/", "namespaces", "/", "guest", "/", "actions",
                  "/", "hello", "?", "blocking", "=", "true"]) :-
    phrase(path(post, guest, actions, hello, [blocking=true]), Xs).

test(query, Xs == ["post", "/", "namespaces", "/", "guest", "/", "actions",
                  "/", "hello", "?", "result", "=", "true"]) :-
    phrase(path(post, guest, actions, hello, [result=true]), Xs).

test(query, Xs == ["post", "/", "namespaces", "/", "guest", "/", "actions",
                  "/", "hello", "?", "timeout", "=", "600"]) :-
    phrase(path(post, guest, actions, hello, [timeout=600]), Xs).

test(query, Xs == ["post", "/", "namespaces", "/", "guest", "/", "actions",
                   "/", "hello", "?", "blocking", "=", "true", "&", "result",
                   "=", "true"]) :-
    phrase(path(post, guest, actions, hello, [blocking=true, result=true]), Xs).

test(query, Xs == ["post", "/", "namespaces", "/", "guest", "/", "actions",
                   "/", "hello", "?", "blocking", "=", "true", "&", "result",
                   "=", "true", "&", "timeout", "=", "600"]) :-
    phrase(path(post, guest, actions, hello,
               [blocking=true, result=true, timeout=600]), Xs).

test(illegal_query, fail) :-
    phrase(path(post, guest, actions, hello, [limit=1]), _Xs).

test(patch, fail) :-
    phrase(path(patch, guest, actions), _Xs).

:- end_tests(actions).
%%

%% Rules Test
:- begin_tests(rules).

test(get, Xs == ["get", "/", "namespaces", "/", "guest", "/", "rules"]) :-
    phrase(path(get, guest, rules), Xs).

test(illegal_verb, fail) :-
    phrase(path(put, guest, rules), _Xs).

test(package_name, Xs == ["get", "/", "namespaces", "/", "guest", "/",
                          "rules", "/", "hello"]) :-
    phrase(path(get, guest, rules, hello), Xs).

test(put, Xs == ["put", "/", "namespaces", "/", "guest", "/", "rules", "/",
                 "hello"]) :-
    phrase(path(put, guest, rules, hello), Xs).

test(delete, Xs == ["delete", "/", "namespaces", "/", "guest", "/", "rules",
                    "/", "hello"]) :-
    phrase(path(delete, guest, rules, hello), Xs).

test(post, Xs == ["post", "/", "namespaces", "/", "guest", "/", "rules",
                  "/", "hello"]) :-
    phrase(path(post, guest, rules, hello), Xs).

:- end_tests(rules).
%%

%%  Triggers Test
:- begin_tests(triggers).

test(get, Xs == ["get", "/", "namespaces", "/", "guest", "/", "triggers"]) :-
    phrase(path(get, guest, triggers), Xs).

test(illegal_verb, fail) :-
    phrase(path(put, guest, triggers), _Xs).

test(package_name, Xs == ["get", "/", "namespaces", "/", "guest", "/",
                          "triggers", "/", "hello"]) :-
    phrase(path(get, guest, triggers, hello), Xs).

test(put, Xs == ["put", "/", "namespaces", "/", "guest", "/", "triggers", "/",
                 "hello"]) :-
    phrase(path(put, guest, triggers, hello), Xs).

test(delete, Xs == ["delete", "/", "namespaces", "/", "guest", "/", "triggers",
                    "/", "hello"]) :-
    phrase(path(delete, guest, triggers, hello), Xs).

test(post, Xs == ["post", "/", "namespaces", "/", "guest", "/", "triggers",
                  "/", "hello"]) :-
    phrase(path(post, guest, triggers, hello), Xs).

:- end_tests(triggers).
%%

%% Activations Test
:- begin_tests(activations).

test(get, Xs == ["get", "/", "namespaces", "/", "guest", "/", "activations"]) :-
    phrase(path(get, guest, activations), Xs).

test(illegal_verb, fail) :-
    phrase(path(put, guest, activations), _Xs).

test(activationid, Xs == ["get", "/", "namespaces", "/", "guest", "/",
                          "activations", "/", "101"]) :-
    phrase(path(get, guest, activations, 101), Xs).

test(resource, Xs == ["get", "/", "namespaces", "/", "guest", "/",
                      "activations", "/", "101", "/", "logs"]) :-
    phrase(path(get, guest, activations, 101, logs), Xs).

test(resource, Xs == ["get", "/", "namespaces", "/", "guest", "/",
                      "activations", "/", "101", "/", "result"]) :-
    phrase(path(get, guest, activations, 101, result), Xs).

test(unknown_resource, fail) :-
    phrase(path(get, guest, activations, 101, unknown), _Xs).

:- end_tests(activations).
%%

%% Packages Test
:- begin_tests(packages).

test(get, Xs == ["get", "/", "namespaces", "/", "guest", "/", "packages"]) :-
    phrase(path(get, guest, packages), Xs).

test(illegal_verb, fail) :-
    phrase(path(put, guest, packages), _Xs).

test(query, Xs == ["get", "/", "namespaces", "/", "guest", "/", "packages",
                   "?", "public", "=", "true"]) :-
    phrase(path(get, guest, packages, none, [public_pkg=true]), Xs).

test(package_name, Xs == ["get", "/", "namespaces", "/", "guest", "/",
                          "packages", "/", "hello"]) :-
    phrase(path(get, guest, packages, hello), Xs).

test(put, Xs == ["put", "/", "namespaces", "/", "guest", "/", "packages", "/",
                 "hello"]) :-
    phrase(path(put, guest, packages, hello), Xs).

test(delete, Xs == ["delete", "/", "namespaces", "/", "guest", "/", "packages",
                    "/", "hello"]) :-
    phrase(path(delete, guest, packages, hello), Xs).

:- end_tests(packages).

%% Namespaces Tests
:- begin_tests(namespaces).

test(get, Xs == ["get", "/", "namespaces"]) :-
    phrase(path(get), Xs).

test(illegal_verb, fail) :-
    phrase(path(put), _Xs).

test(get, Xs == ["get", "/", "namespaces", "/", "guest"]) :-
    phrase(path(get, guest), Xs).

:- end_tests(namespaces).
%%
