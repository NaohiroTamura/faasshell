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
              term_json_dict/2,
              json_path_merge/5,
              json_path_value/5
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
    setup_call_cleanup(
            open(File, read, S, []),
            json_read_dict(S, Asl, []),
            close(S)).

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

%%
json_path_merge(_Json, Value, [], Value).
json_path_merge(Json, Value, [H|T], MergedJson) :-
    json_path_merge(_{}, Value, T, J1),
    MergedJson = Json.put(H, J1).
%% json_path_merge(+JsonPath, +Json, +Value, -Keys, -MergedJson)
json_path_merge(JsonPath, Json, Value, Keys, MergedJson) :-
    tokenize_atom(JsonPath, Token),
    json_path(Keys, _Range, Token, []),
    json_path_merge(Json, Value, Keys, MergedJson).

%%
json_path_value(Value, [], Range, Value) :-
    var(Range), !.
json_path_value(List, [], [I1, I2], Value) :-
    is_list(List),
    slice(List, I1, I2, Value).
json_path_value(Json, [H|T], Range, Value) :-
    J1 = Json.H,
    json_path_value(J1, T, Range, Value).
%% json_path_value(+JsonPath, +Json, -Keys, -Range, -Value)
json_path_value(JsonPath, Json, Keys, Range, Value) :-
    tokenize_atom(JsonPath, Token),
    json_path(Keys, Range, Token, []),
    json_path_value(Json, Keys, Range, Value).

%%
json_path(Keys, Range)   --> root, keys(Keys, Range).
keys([], _)              --> [].
keys([H|T], Range)       --> key(H, Range), keys(T, Range).
key(H, Range)            --> plain_key(H, Range) | range_key(H, Range).
plain_key(Key, _Range)   --> dot, word(Key).
range_key(Key, [I1, I2]) --> dot, word(Key), range(I1, I2).
range(I1, I2)            --> lpar, [I1], colon, [I2], rpar.
word(Key)   --> [X], ['_'], word(Y), {atomic_list_concat([X, '_', Y], Key) }
              | [Key].
root  --> ['$'].
dot   --> ['.'].
colon --> [':'].
lpar  --> ['['].
rpar  --> [']'].

%%
slice([X|_],0,1,[X]).
slice([X|Xs],0,K,[X|Ys]) :- K > 0,
   K1 is K - 1, slice(Xs,0,K1,Ys).
slice([_|Xs],I,K,Ys) :- I > 0,
   I1 is I - 1, K1 is K - 1, slice(Xs,I1,K1,Ys).
