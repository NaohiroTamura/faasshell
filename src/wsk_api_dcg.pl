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

:- module(wsk_api_dcg,
          [
            path//1,
            path//2,
            path//3,
            path//4,
            path//5
         ]).

%% OpenWhisk REST API Grammar
path(get)     --> verb(get), namespaces.
path(get,N)   --> verb(get), namespaces, namespace(N).
path(get,N,A) --> verb(get), namespaces, namespace(N), apis(A).
path(V,N,A,E) --> verb(V),   namespaces, namespace(N), apis(A), entity(E).
path(V,N,A,E,[]) --> path(V,N,A,E).
%% Actions
path(get,N,actions,E,Q)  --> path(get,N,actions,E),  ["?"], query(get, Q).
path(put,N,actions,E,Q)  --> path(put,N,actions,E),  ["?"], query(put, Q).
path(post,N,actions,E,Q) --> path(post,N,actions,E), ["?"], query(post, Q).
%% Activations
path(get,N,activations,E,Q) --> path(get,N,activations,E), ["?"], query(get, Q).
path(get,N,activations,E,R) --> path(get,N,activations,E), resource(R).
%% Packages
path(get,N,packages,E,Q)   --> path(get,N,packages,E), ["?"], query(get, Q).

%%
query(V, [Q])    --> param(V, Q), !.
query(V, [Q|Qs]) --> param(V, Q), ["&"], query(V, Qs).

%%
%% terminals
%%
verb(get)     --> ["get"].
verb(put)     --> ["put"].
verb(delete)  --> ["delete"].
verb(post)    --> ["post"].
verb(patch)   --> { fail }.

namespaces    --> ["/", "namespaces"].

namespace(default) --> ["/_"], !.
namespace(N)       --> ["/", S], { atom_string(N, S) }.

apis(actions)     --> ["/", "actions"].
apis(rules)       --> ["/", "rules"].
apis(triggers)    --> ["/", "triggers"].
apis(activations) --> ["/", "activations"].
apis(packages)    --> ["/", "packages"].

entity(none) --> [].
entity(E)    --> ["/", S], { atom_string(E, S) }.

param(get, name=B)      --> ["name", "=", S], { atom_string(B,S) }.
param(get, limit=N)     --> ["limit", "=", S], { number_string(N,S) }.
param(get, skip=N)      --> ["skip", "=", S], { number_string(N,S) }.
param(get, since=N)     --> ["since", "=", S], { number_string(N,S) }.
param(get, upto=N)      --> ["upto", "=", S], { number_string(N,S) }.
param(get, docs=B)      --> ["docs", "=", S], { atom_string(B,S) }.
% public_pkg, but not public, because of built-in public/1
param(get, public_pkg=B)--> ["public", "=", S], { atom_string(B,S) }.
param(put, overwrite=B) --> ["overwrite", "=", S], { atom_string(B,S) }.
param(post, blocking=B) --> ["blocking", "=", S], { atom_string(B,S) }.
param(post, result=B)   --> ["result", "=", S], { atom_string(B,S)  }.
param(post, timeout=N)  --> ["timeout", "=", S], { number_string(N,S) }.
param(post, state=B)    --> ["state", "=", S], { atom_string(B,S)  }.

resource(logs)   --> ["/", "logs"].
resource(result) --> ["/", "result"].
%% end of OpenWhisk REST API Grammar
