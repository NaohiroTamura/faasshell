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

%% Utils
faasshell_api_host(HOST) :-
    getenv('FAASSHELL_APIHOST', HOST), HOST \== '', !.
faasshell_api_host('https://127.0.0.1:8443').
%% faasshell_api_host('http://127.0.0.1:8080').
faasshell_api_key('ec29e90c-188d-11e8-bb72-00163ec1cd01'-'0b82fe63b6bd450519ade02c3cb8f77ee581f25a810db28f3910e6cdd9d041bf').

load_json(File, Term) :-
    atomic_list_concat(['envsubst <', File], Command),
    setup_call_cleanup(
            open(pipe(Command), read, S),
            json_read(S, Term, []),
            close(S)).

read_version(Version) :-
    Command = 'git log -n 1 --date=short --format=format:"rev.%ad.%h" HEAD',
    setup_call_cleanup(
            open(pipe(Command), read, S),
            read_string(S, _L, Ver),
            close(S)),
    atomics_to_string(['$Id ', Ver, ' $'], Version).

term_json_dict(Term, Dict) :-
    ground(Term), !,
    atom_json_term(Atom, Term, []), atom_json_dict(Atom, Dict, []).
term_json_dict(Term, Dict) :-
    atom_json_dict(Atom, Dict, []), atom_json_term(Atom, Term, []).
