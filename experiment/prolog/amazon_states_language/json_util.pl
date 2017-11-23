%% -*- mode: prolog; coding: utf-8; -*-
%%
%% $Id$
%%

%%:- module(json_util,
%%          [ 
%%         ]).

:- use_module(library(http/json)).

load_json(File, Asl) :-
    open(File, read, S, []),
    json_read_dict(S, Asl, []),
    close(S).

search_dic(D, K, V, [K]) :-
    catch( V = D.K, _, false ),
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
