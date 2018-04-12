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

:- module(faasshell_repl,
          [ repl/0
          ]).

:- use_module(library(readline)).

:- use_module(faasshell_run).

:- set_prolog_flag(verbose, silent).
:- set_prolog_flag(toplevel_prompt, 'faasshell debug> ').
:- set_prolog_flag(readline, readline).

:- initialization(repl).

repl :-
    debug(repl > user_error),
    set_setting(http:logfile,'/logs/httpd.log'), % docker volume /tmp
    prompt(_OldPrompt, 'faasshell> '),
    current_prolog_flag(readline, Readline),
    debug(repl, 'repl: ~w', [readline(Readline)]),
    load_history,
    repl_loop.

repl_loop :-
    read_term(Term, [variable_names(Vars)]),
    debug(repl, 'repl: ~w, ~w', [term(Term), vars(Vars)]),
    ( Term == end_of_file
      -> save_history, !
      ;  term_to_atom(Term, Atom),
         debug(repl, 'repl: ~w', [atom(Atom)]),
         rl_add_history(Atom),
         phrase(tuple_list(Term), Dsl),
         debug(repl, 'repl: ~w', [fsm(Dsl)]),
         faasshell_run:start(fsm(Dsl), [], I, O),
         debug(repl, 'repl: ~w, ~w', [input(I), output(O)]),
         writeln(Vars),
         repl_loop
    ).

tuple_list((A,B)) --> !, tuple_list(A), tuple_list(B).
tuple_list(I)     --> { compound(I) }, [I].

history_file(File) :-
    getenv('HOME', HOME),
    atomic_list_concat([HOME, '.faasshell_history'], '/', File).

load_history :-
    history_file(File),
    access_file(File, read)
    -> rl_read_history(File)
    ;  true.

save_history :-
    history_file(File),
    access_file(File, write)
    -> rl_write_history(File)
    ;  true.
