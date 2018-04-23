#!/usr/bin/env swipl
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
:- current_prolog_flag(emacs_inferior_process, true)
   -> set_prolog_flag(color_term, false),
      set_prolog_flag(readline, false)
   ;  stream_property(current_output, tty(true))
      -> set_prolog_flag(readline, readline)
      ;  set_prolog_flag(readline, false).

:- initialization(repl).

repl :-
    %debug(repl > user_error),
    set_setting(http:logfile,'/logs/httpd.log'), % docker volume /tmp
    prompt(_OldPrompt, '| '),
    current_prolog_flag(readline, Readline),
    debug(repl, 'repl: ~w', [readline(Readline)]),
    load_history,
    repl_loop(_{}).

repl_loop(EI) :-
    prompt1('faasshell> '),
    ( read_term(Term, [variable_names(Vars), syntax_errors(fail)])
      -> debug(repl, 'repl: ~w, ~w', [term(Term), vars(Vars)])
      ;  repl_loop(EI)
    ),
    ( Term == end_of_file
      -> save_history, !
      ;  phrase(faasshell_run:tuple_list(Term), Dsl),
         debug(repl, 'repl: ~w', [dsl(Dsl)]),

         replace_logical_var(Vars, Term, Atom),
         debug(repl, 'repl: ~w', [atom(Atom)]),
         ( rl_add_history(Atom)
           -> true
           ;  debug(repl, 'repl: dup ~w', [rl_add_history(Atom)])
         ),

         catch( ( faasshell_run:start(fsm(Dsl), [], I, O, EI, EO)
                  -> true
                  ;  debug(repl, 'repl: failed ~w', [start(fsm(Dsl), I, O, EI)]),
                     nonvar(O) -> true; O = false
                ),
                Error,
                print_message(error, Error)
              ),
         debug(repl, 'repl: ~w, ~w, ~w', [input(I), output(O), env(EO)]),
         format('Output=~w~n', [O]),
         maplist(writeln, Vars),
         repl_loop(EO)
    ).

replace_logical_var(Variables, CommandTerm, CommandReplaced) :-
    term_to_atom(CommandTerm, CommandAtom),
    foldl([(X=LogicalVar),Y,Z]>>(
              term_to_atom(LogicalVar, LogicalVarAtom),
              re_replace(LogicalVarAtom/ga, X, Y, Z)
          ), Variables, CommandAtom, CommandReplaced).

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
