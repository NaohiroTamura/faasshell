%% -*- mode: prolog; coding: utf-8; -*-
%%
%% $Id$
%%
%% $ swipl -o exec -g main --stand_alone=true -c asl_validate.pl
%%

:- use_module(library(http/json)).

debugln(_).
%%debugln(Msg) :- writeln(Msg).

graphdot([]).
graphdot([A>B|Gs]) :-
    format("     ~w -> ~w ;~n", [A,B]),
    graphdot(Gs).
graphviz(Graph) :- 
    writeln("digraph graph_name {"),
    list_to_set(Graph, GraphSet),
    graphdot(GraphSet),
    writeln("}").

main(File, Graph) :-
    load_json(File, Asl),
    parse(Asl, Graph),
    %
    %json_write_dict(user_output, Asl, []),
    debugln("Done!").
    %halt.

load_json(File, Asl) :-
    open(File, read, S, []),
    json_read_dict(S, Asl, []),
    close(S).

parse(Asl, Graph) :-
    %Asl = _{'Comment':Comment, 'StartAt':StartAt, 'States':States},
    _{'StartAt':StartAt, 'States':States} :< Asl,
    string(StartAt),
    atom_string(StartAtKey, StartAt),
    debugln(start(state(StartAt))),
    parse(States, StartAtKey, G1),
    Graph = ['Start'>StartAtKey | G1].

parse(States, StateKey, Graph) :-
    _{'Type':"Pass",'Next':Next} :< States.StateKey,
    string(Next),
    atom_string(NextKey, Next),
    debugln((state(StateKey):type(pass):next(Next))),
    parse(States, NextKey, G1),
    Graph = [StateKey>NextKey | G1].

parse(States, StateKey, Graph) :-
    _{'Type':"Task", 'Resource':Resource, 'Next':Next} :< States.StateKey,
    string(Next),
    atom_string(NextKey, Next),
    debugln((state(StateKey):type(task):next(Next))),
    parse(States, NextKey, G1),
    Graph = [StateKey>NextKey | G1].

parse(States, StateKey, [StateKey>'End']) :-
    _{'Type':"Task", 'Resource':Resource, 'End':End} :< States.StateKey,
    debugln((state(StateKey):type(task):end(End))).

parse(States, StateKey, Graph) :-
    _{'Type':"Choice", 'Choices':Choices} :< States.StateKey,
    choices(States, StateKey, Choices, G1),
    (  _{'Default':Default} :< States.StateKey
    -> string(Default),
       atom_string(DefaultKey, Default),
       debugln((state(StateKey):type(choice):default(Default))),
       parse(States, DefaultKey, G2),
       append(G1, [StateKey>DefaultKey | G2], Graph)
    ; Graph = G1).

parse(States, StateKey, Graph) :-
    _{'Type':"Wait", 'Next':Next} :< States.StateKey,
    string(Next),
    atom_string(NextKey, Next),
    debugln((state(StateKey):type(wait):next(Next))),
    parse(States, NextKey, G1),
    Graph = [StateKey>NextKey | G1].

parse(States, StateKey, [StateKey>'Succeed']) :-
    _{'Type':"Succeed"} :< States.StateKey,
    debugln((state(StateKey):type(succeed))).

parse(States, StateKey, [StateKey>'Fail']) :-
    _{'Type':"Fail"} :< States.StateKey,
    write((state(StateKey):type(fail))),
    (_{'Cause':Cause} :< States.StateKey -> write(:),write(cause(Cause)); true),
    (_{'Error':Error} :< States.StateKey -> write(:),write(error(Error)); true),
    debugln(.).

parse(States, StateKey, Graph) :-
    _{'Type':"Parallel",'Branches':Branches,'Next':Next} :< States.StateKey,
    branches(States, StateKey, Branches),
    string(Next),
    atom_string(NextKey, Next),
    debugln((state(StateKey):type(parallel):next(Next))),
    parse(States, NextKey, G1),
    Graph = [StateKey>NextKey | G1].

parse(States, StateKey,  Graph) :-
    _{'Type':"Parallel",'Branches':Branches,'End':End} :< States.StateKey,
    branches(States, StateKey, Branches, G1),
    Graph = [StateKey>'End' | G1],
    debugln((state(StateKey):type(parallel):end(End))).

choices(_States, _StateKey, [], []).
choices(States, StateKey, [C|Cs], Graph) :-
    _{'Next':Next} :< C,
    string(Next),
    atom_string(NextKey, Next),
    debugln((state(StateKey):type(choice):next(Next))),
    parse(States, NextKey, G1),
    choices(States, StateKey, Cs, G2),
    append([StateKey>NextKey | G1], G2, Graph).

branches(_States, _StateKey, [], []).
branches(States, StateKey, [B|Bs], Graph) :-
    _{'StartAt':StartAt,'States':PStates} :< B,
    string(StartAt),
    atom_string(StartAtKey, StartAt),
    debugln((state(StateKey):type(parallel):startat(StartAt))),
    parse(PStates, StartAtKey, G1),
    branches(States, StateKey, Bs, G2),
    append([PStates>StartAtKey | G1], G2, Graph).

%%
%% Unit Tests
%%
:- use_module(library(plunit)).

test1 :- main('example.json').
test2 :- main('test/has-dupes.json').
test3 :- main('test/linked-parallel.json').
test4 :- main('test/minimal-fail-state.json').
test5 :- main('test/no-terminal.json').
