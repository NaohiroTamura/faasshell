%% -*- mode: prolog; coding: utf-8; -*-
%%
%% $Id$
%%
%% $ swipl -o exec -g main --stand_alone=true -c asl_validate.pl

:- use_module(library(http/json)).
%:- use_module(library(http/json_convert)).
%:- use_module(library(http/http_json)).

test1 :- main('example.json').
test2 :- main('test/has-dupes.json').
test3 :- main('test/linked-parallel.json').
test4 :- main('test/minimal-fail-state.json').
test5 :- main('test/no-terminal.json').

main(File) :-
    load_json(File,Asl),
    parse(Asl),
    %
    %json_write_dict(user_output,Asl,[]),
    writeln("Done!").
    %halt.

load_json(File,Asl) :-
    open(File,read,S,[]),
    json_read_dict(S,Asl,[]),
    close(S).

parse(Asl) :-
    %Asl = _{'Comment':Comment,'StartAt':StartAt,'States':States},
    _{'StartAt':StartAt,'States':States} :< Asl,
    string(StartAt),
    atom_string(StartAtKey, StartAt),
    writeln(start(state(StartAt))),
    parse(States,StartAtKey).

parse(States,StateKey) :-
    _{'Type':"Pass",'Next':Next} :< States.StateKey,
    string(Next),
    atom_string(NextKey,Next),
    writeln((state(StateKey):type(pass):next(Next))),
    parse(States,NextKey).

parse(States,StateKey) :-
    _{'Type':"Task",'Resource':Resource,'Next':Next} :< States.StateKey,
    string(Next),
    atom_string(NextKey,Next),
    writeln((state(StateKey):type(task):next(Next))),
    parse(States,NextKey).

parse(States,StateKey) :-
    _{'Type':"Task",'Resource':Resource,'End':End} :< States.StateKey,
    writeln((state(StateKey):type(task):end(End))).

parse(States,StateKey) :-
    _{'Type':"Choice",'Choices':Choices} :< States.StateKey,
    choices(States,StateKey,Choices),
    (  _{'Default':Default} :< States.StateKey
    -> string(Default),
       atom_string(DefaultKey,Default),
       writeln((state(StateKey):type(choice):default(Default))),
       parse(States,DefaultKey)
    ; true).

parse(States,StateKey) :-
    _{'Type':"Wait",'Next':Next} :< States.StateKey,
    string(Next),
    atom_string(NextKey, Next),
    writeln((state(StateKey):type(wait):next(Next))),
    parse(States,NextKey).

parse(States,StateKey) :-
    _{'Type':"Succeed"} :< States.StateKey,
    writeln((state(StateKey):type(succeed))).

parse(States,StateKey) :-
    _{'Type':"Fail"} :< States.StateKey,
    write((state(StateKey):type(fail))),
    (_{'Cause':Cause} :< States.StateKey -> write(:),write(cause(Cause)); true),
    (_{'Error':Error} :< States.StateKey -> write(:),write(error(Error)); true),
    writeln(.).

parse(States,StateKey) :-
    _{'Type':"Parallel",'Branches':Branches,'Next':Next} :< States.StateKey,
    branches(States,StateKey,Branches),
    string(Next),
    atom_string(NextKey,Next),
    writeln((state(StateKey):type(parallel):next(Next))),
    parse(States,NextKey).

parse(States,StateKey) :-
    _{'Type':"Parallel",'Branches':Branches,'End':End} :< States.StateKey,
    branches(States,StateKey,Branches),
    writeln((state(StateKey):type(parallel):end(End))).

choices(_States,_StateKey,[]).
choices(States,StateKey,[C|Cs]) :-
    _{'Next':Next} :< C,
    string(Next),
    atom_string(NextKey,Next),
    writeln((state(StateKey):type(choice):next(Next))),
    parse(States,NextKey),
    choices(States,StateKey,Cs).

branches(_States,_StateKey,[]).
branches(States,StateKey,[B|Bs]) :-
    _{'StartAt':StartAt,'States':PStates} :< B,
    string(StartAt),
    atom_string(StartAtKey,StartAt),
    writeln((state(StateKey):type(parallel):startat(StartAt))),
    parse(PStates,StartAtKey),
    branches(States,StateKey,Bs).
