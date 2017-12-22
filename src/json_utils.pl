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

:- module(json_utils,
          [
              term_json_dict/2
         ]).

:- use_module(library(http/json)).

%%
term_json_dict(Term, Dict) :-
    ground(Term), !,
    atom_json_term(Atom, Term, []), atom_json_dict(Atom, Dict, []).
term_json_dict(Term, Dict) :-
    atom_json_dict(Atom, Dict, []), atom_json_term(Atom, Term, []).

%%
load_json(File, Asl) :-
    open(File, read, S, []),
    json_read_dict(S, Asl, []),
    close(S).

%%
search_dic(D, K, V, [K]) :-
    catch( V = D.K, _, false ).
    %%writeln(case1(dic(D),key(K),value(V))).
search_dic(D, K, V, [K1|P]) :-
    V1 = D.K1, is_dict(V1), search_dic(V1, K, V, P).
search_dic(D, K, V, [K1|P]) :-
    V1 = D.K1, is_list(V1), member(N, V1), search_dic(N, K, V, P).
%% ?- load_json('test/no-terminal.json',J), 
%%    findall((P,V),search_dic(J,'Type',V,P),L).
%% L = [(['States', 'FirstState', 'Type'], "Task"),
%%      (['States', 'ChoiceState', 'Type'], "Choice"),
%%      (['States', 'FirstMatchState', 'Type'], "Task"), 
%%      (['States', 'SecondMatchState', 'Type'], "Task"), 
%%      (['States', 'NextState', 'Type'], "Task")].
