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
          [ shell_main/0
          ]).

:- use_module(faasshell_run).

:- initialization(shell_main).

shell_main :- 
    set_prolog_flag(verbose, silent),
    set_prolog_flag(toplevel_prompt, 'faasshell debug> '),
    prompt(_OldPrompt, 'faasshell> '),
    debug(repl > user_error),
    repl.

repl :-
    read_term(Term, []),
    debug(repl, 'repl: ~w', [term(Term)]),
    ( Term == end_of_file
      -> !
      ;  phrase(tuple_list(Term), Dsl),
         debug(repl, 'repl: ~w', [fsm(Dsl)]),
         faasshell_run:start(fsm(Dsl), [], I, O),
         debug(repl, 'repl: ~w, ~w', [input(I), output(O)]),
         repl
    ).

tuple_list((A,B)) --> !, tuple_list(A), tuple_list(B).
tuple_list(I)     --> { compound(I) }, [I].

