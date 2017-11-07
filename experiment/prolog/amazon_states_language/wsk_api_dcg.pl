%% -*- mode: prolog; coding: utf-8; -*-
%%
%% $Id$
%%

%%:- module(wsk_api, []).

%% OpenWhisk REST API Grammar
url(get)     --> verb(get), namespaces.
url(get,N)   --> verb(get), namespaces, namespace(N).
url(get,N,A) --> verb(get), namespaces, namespace(N), apis(A).
url(V,N,A,E) --> verb(V),   namespaces, namespace(N), apis(A), entity(E).
%% Actions
url(get,N,actions,E,Q)  --> url(get,N,actions,E),  ["?"], query(get, Q).
url(post,N,actions,E,Q) --> url(post,N,actions,E), ["?"], query(post, Q).
%% Activations
url(get,N,activations,Q)   --> url(get,N,activations), ["?"], query(get, Q).
url(get,N,activations,E,R) --> url(get,N,activations,E), resource(R).

%%
query(V, [Q])    --> param(V, Q), !.
query(V, [Q|Qs]) --> param(V, Q), ["&"], query(V, Qs).

verb(get)     --> ["get"].
verb(put)     --> ["put"].
verb(delete)  --> ["delete"].
verb(post)    --> ["post"].
verb(patch)   --> { fail }.

namespaces    --> ["/", "namespaces"].
namespace(N)  --> ["/", S], { atom_string(N, S) }.

apis(actions)     --> ["/", "actions"].
apis(rules)       --> ["/", "rules"].
apis(triggers)    --> ["/", "triggers"].
apis(activations) --> ["/", "activations"].
apis(packages)    --> ["/", "packages"].

entity(E)    --> ["/", S], { atom_string(E, S) }.

param(get, limit=N)     --> ["limit", "=", S], { number_string(N,S) }.
param(get, skip=N)      --> ["skip", "=", S], { number_string(N,S) }.
param(get, since=N)     --> ["since", "=", S], { number_string(N,S) }.
param(get, upto=N)      --> ["upto", "=", S], { number_string(N,S) }.
param(get, docs=B)      --> ["docs", "=", S], { atom_string(B,S) }.
param(post, blocking=B) --> ["blocking", "=", S], { atom_string(B,S) }.
param(post, result=B)   --> ["result", "=", S], { atom_string(B,S)  }.
param(post, timeout=N)  --> ["timeout", "=", S], { number_string(N,S) }.

resource(logs)   --> ["/", "logs"].
resource(result) --> ["/", "result"].

%%
%% Unit Tests
%%
:- use_module(library(plunit)).

%% Actions Test
:- begin_tests(actions).

test(get, Xs == ["get", "/", "namespaces", "/", "guest", "/", "actions"]) :-
    phrase(url(get, guest, actions), Xs).

test(get_rev, (V, N, A) == (get, guest, actions)) :-
    phrase(url(V, N, A), ["get", "/", "namespaces", "/", "guest", "/", "actions"]).

test(illegal_verb, fail) :-
    phrase(url(put, guest, actions), _Xs).

test(action_name, Xs == ["get", "/", "namespaces", "/", "guest", "/", "actions",
                         "/", "hello"]) :-
    phrase(url(get, guest, actions, hello), Xs).

test(action_name_rev, (V, N, A, E) == (get, guest, actions, hello)) :-
    phrase(url(V, N, A, E),
           ["get", "/", "namespaces", "/", "guest", "/", "actions", "/", "hello"]).

test(query, Xs == ["get", "/", "namespaces", "/", "guest", "/", "actions",
                   "/", "hello", "?", "limit", "=", "1"]) :-
    url(get, guest, actions, hello, [limit=1], Xs, []).

test(query_rev, (V, N, A, E, Q)  == (get, guest, actions, hello, [limit=1])) :-
    url(V, N, A, E, Q, ["get", "/", "namespaces", "/", "guest", "/", "actions",
                        "/", "hello", "?", "limit", "=", "1"], []).

test(query, Xs == ["get", "/", "namespaces", "/", "guest", "/", "actions",
                   "/", "hello", "?", "skip", "=", "2"]) :-
    url(get, guest, actions, hello, [skip=2], Xs, []).

test(query, Xs == ["get", "/", "namespaces", "/", "guest", "/", "actions",
                   "/", "hello", "?", "limit", "=", "1", "&", "skip", "=", "2"]) :-
    url(get, guest, actions, hello, [limit=1, skip=2], Xs, []).

test(unknown_query, fail) :-
    url(get, guest, actions, hello, [unknown=true], _Xs, []).

test(put, Xs == ["put", "/", "namespaces", "/", "guest", "/", "actions",
                 "/", "hello"]) :-
    phrase(url(put, guest, actions, hello), Xs).

test(delete, Xs == ["delete", "/", "namespaces", "/", "guest", "/", "actions",
                    "/", "hello"]) :-
    phrase(url(delete, guest, actions, hello), Xs).

test(post, Xs == ["post", "/", "namespaces", "/", "guest", "/", "actions",
                  "/", "hello"]) :-
    phrase(url(post, guest, actions, hello), Xs).

test(query, Xs == ["post", "/", "namespaces", "/", "guest", "/", "actions",
                  "/", "hello", "?", "blocking", "=", "true"]) :-
    phrase(url(post, guest, actions, hello, [blocking=true]), Xs]).

test(query, Xs == ["post", "/", "namespaces", "/", "guest", "/", "actions",
                  "/", "hello", "?", "result", "=", "true"]) :-
    phrase(url(post, guest, actions, hello, [result=true]), Xs).

test(query, Xs == ["post", "/", "namespaces", "/", "guest", "/", "actions",
                  "/", "hello", "?", "timeout", "=", "600"]) :-
    phrase(url(post, guest, actions, hello, [timeout=600]), Xs).

test(query, Xs == ["post", "/", "namespaces", "/", "guest", "/", "actions",
                   "/", "hello", "?", "blocking", "=", "true", "&", "result",
                   "=", "true"]) :-
    phrase(url(post, guest, actions, hello, [blocking=true, result=true]), Xs).

test(query, Xs == ["post", "/", "namespaces", "/", "guest", "/", "actions",
                   "/", "hello", "?", "blocking", "=", "true", "&", "result",
                   "=", "true", "&", "timeout", "=", "600"]) :-
    phrase(url(post, guest, actions, hello,
               [blocking=true, result=true, timeout=600]), Xs).

test(illegal_query, fail) :-
    phrase(url(post, guest, actions, hello, [limit=1]), _Xs).

test(patch, fail) :-
    phrase(url(patch, guest, actions), _Xs).

:- end_tests(actions).
%%

%% Rules Test
:- begin_tests(rules).

test(get, Xs == ["get", "/", "namespaces", "/", "guest", "/", "rules"]) :-
    phrase(url(get, guest, rules), Xs).

test(illegal_verb, fail) :-
    phrase(url(put, guest, rules), _Xs).

test(package_name, Xs == ["get", "/", "namespaces", "/", "guest", "/",
                          "rules", "/", "hello"]) :-
    phrase(url(get, guest, rules, hello), Xs).

test(put, Xs == ["put", "/", "namespaces", "/", "guest", "/", "rules", "/",
                 "hello"]) :-
    phrase(url(put, guest, rules, hello), Xs).

test(delete, Xs == ["delete", "/", "namespaces", "/", "guest", "/", "rules",
                    "/", "hello"]) :-
    phrase(url(delete, guest, rules, hello), Xs).

test(post, Xs == ["post", "/", "namespaces", "/", "guest", "/", "rules",
                  "/", "hello"]) :-
    phrase(url(post, guest, rules, hello), Xs).

:- end_tests(rules).
%%

%%  Triggers Test
:- begin_tests(triggers).

test(get, Xs == ["get", "/", "namespaces", "/", "guest", "/", "triggers"]) :-
    phrase(url(get, guest, triggers), Xs).

test(illegal_verb, fail) :-
    phrase(url(put, guest, triggers), _Xs).

test(package_name, Xs == ["get", "/", "namespaces", "/", "guest", "/",
                          "triggers", "/", "hello"]) :-
    phrase(url(get, guest, triggers, hello), Xs).

test(put, Xs == ["put", "/", "namespaces", "/", "guest", "/", "triggers", "/",
                 "hello"]) :-
    phrase(url(put, guest, triggers, hello), Xs).

test(delete, Xs == ["delete", "/", "namespaces", "/", "guest", "/", "triggers",
                    "/", "hello"]) :-
    phrase(url(delete, guest, triggers, hello), Xs).

test(post, Xs == ["post", "/", "namespaces", "/", "guest", "/", "triggers",
                  "/", "hello"]) :-
    phrase(url(post, guest, triggers, hello), Xs).

:- end_tests(triggers).
%%

%% Activations Test
:- begin_tests(activations).

test(get, Xs == ["get", "/", "namespaces", "/", "guest", "/", "activations"]) :-
    phrase(url(get, guest, activations), Xs).

test(illegal_verb, fail) :-
    phrase(url(put, guest, activations), _Xs).

test(activationid, Xs == ["get", "/", "namespaces", "/", "guest", "/",
                          "activations", "/", "101"]) :-
    phrase(url(get, guest, activations, 101), Xs).

test(resource, Xs == ["get", "/", "namespaces", "/", "guest", "/",
                      "activations", "/", "101", "/", "logs"]) :-
    phrase(url(get, guest, activations, 101, logs), Xs).

test(resource, Xs == ["get", "/", "namespaces", "/", "guest", "/",
                      "activations", "/", "101", "/", "result"]) :-
    phrase(url(get, guest, activations, 101, result), Xs).

test(unknown_resource, fail) :-
    phrase(url(get, guest, activations, 101, unknown), _Xs).

:- end_tests(activations).
%%

%% Packages Test
:- begin_tests(packages).

test(get, Xs == ["get", "/", "namespaces", "/", "guest", "/", "packages"]) :-
    phrase(url(get, guest, packages), Xs).

test(illegal_verb, fail) :-
    phrase(url(put, guest, packages), _Xs).

test(package_name, Xs == ["get", "/", "namespaces", "/", "guest", "/",
                          "packages", "/", "hello"]) :-
    phrase(url(get, guest, packages, hello), Xs).

test(put, Xs == ["put", "/", "namespaces", "/", "guest", "/", "packages", "/",
                 "hello"]) :-
    phrase(url(put, guest, packages, hello), Xs).

test(delete, Xs == ["delete", "/", "namespaces", "/", "guest", "/", "packages",
                    "/", "hello"]) :-
    phrase(url(delete, guest, packages, hello), Xs).

:- end_tests(packages).

%% Namespaces Tests
:- begin_tests(namespaces).

test(get, Xs == ["get", "/", "namespaces"]) :-
    phrase(url(get), Xs).

test(illegal_verb, fail) :-
    phrase(url(put), _Xs).

test(get, Xs == ["get", "/", "namespaces", "/", "guest"]) :-
    phrase(url(get, guest), Xs).

:- end_tests(namespaces).
%%
