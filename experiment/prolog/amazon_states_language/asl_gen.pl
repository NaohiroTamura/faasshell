%% -*- mode: prolog; coding: utf-8; -*-
%%
%% $Id$
%%
%% $ swipl -o exec -g main --stand_alone=true -c asl_validate.pl
%%

:- use_module(library(http/json)).

gencode(Msg) :- writeln(Msg).

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
    parse(Asl, Graph).

load_json(File, Asl) :-
    open(File, read, S, []),
    json_read_dict(S, Asl, []),
    close(S).

parse(Asl, Graph) :-
    %Asl = _{'Comment':Comment, 'StartAt':StartAt, 'States':States},
    _{'StartAt':StartAt, 'States':States} :< Asl,
    string(StartAt),
    atom_string(StartAtKey, StartAt),
    gencode( 'dsl( ' ),
    parse(States, StartAtKey, G1),
    Graph = ['Start'>StartAtKey | G1],
    gencode( ' ) ' ).
    
parse(States, StateKey, Graph) :-
    _{'Type':"Pass",'Next':Next} :< States.StateKey,
    string(Next),
    atom_string(NextKey, Next),
    gencode(pass(StateKey)),
    parse(States, NextKey, G1),
    Graph = [StateKey>NextKey | G1].

parse(States, StateKey, Graph) :-
    _{'Type':"Task", 'Resource':Resource, 'Next':Next} :< States.StateKey,
    string(Next),
    atom_string(NextKey, Next),
    gencode(task(StateKey, Resource)),
    parse(States, NextKey, G1),
    Graph = [StateKey>NextKey | G1].

parse(States, StateKey, [StateKey>'End']) :-
    _{'Type':"Task", 'Resource':Resource, 'End':End} :< States.StateKey,
    gencode(task(StateKey, Resource)).

parse(States, StateKey, Graph) :-
    _{'Type':"Choice", 'Choices':Choices} :< States.StateKey,
    gencode('choices( ' ),
    choices(States, StateKey, Choices, G1),
    (  _{'Default':Default} :< States.StateKey
    -> string(Default),
       atom_string(DefaultKey, Default),
       gencode('default( '),
       parse(States, DefaultKey, G2),
       gencode(' ) '),
       append(G1, [StateKey>DefaultKey | G2], Graph)
     ; Graph = G1),
    gencode(' ) ' ).

parse(States, StateKey, Graph) :-
    _{'Type':"Wait", 'Next':Next} :< States.StateKey,
    string(Next),
    atom_string(NextKey, Next),
    gencode(wait(StateKey)),
    parse(States, NextKey, G1),
    Graph = [StateKey>NextKey | G1].

parse(States, StateKey, [StateKey>'Succeed']) :-
    _{'Type':"Succeed"} :< States.StateKey,
    gencode(succeed(StateKey)).

parse(States, StateKey, [StateKey>'Fail']) :-
    _{'Type':"Fail"} :< States.StateKey,
    (_{'Error':Error} :< States.StateKey -> true; Error = unknown),
    (_{'Cause':Cause} :< States.StateKey -> true; Cause = unknown),
    gencode(fail(StateKey, Error, Cause)).

parse(States, StateKey, Graph) :-
    _{'Type':"Parallel",'Branches':Branches,'Next':Next} :< States.StateKey,
    gencode(parallel(StateKey)),
    gencode( 'branches( ' ),
    branches(States, StateKey, Branches, G1),
    gencode( ' ), ' ),
    string(Next),
    atom_string(NextKey, Next),
    parse(States, NextKey, G2),
    append(G1, [StateKey>NextKey | G2], Graph).


parse(States, StateKey,  Graph) :-
    _{'Type':"Parallel",'Branches':Branches,'End':End} :< States.StateKey,
    gencode(parallel(StateKey)),
    gencode( 'branches( ' ),
    branches(States, StateKey, Branches, G1),
    gencode( ' 7) ' ),
    Graph = [StateKey>'End' | G1],
    gencode( ' 8) ' ).

choices(_States, _StateKey, [], []).
choices(States, StateKey, [C|Cs], Graph) :-
    _{'Next':Next, 'Variable': Variable, 'NumericEquals': Num} :< C,
    gencode( if(Variable == Num) ),
    gencode( 'then (' ),
    string(Next),
    atom_string(NextKey, Next),
    parse(States, NextKey, G1),
    gencode( ' ) '),
    choices(States, StateKey, Cs, G2),
    append([StateKey>NextKey | G1], G2, Graph).

branches(_States, _StateKey, [], []).
branches(States, StateKey, [B|Bs], Graph) :-
    _{'StartAt':StartAt,'States':PStates} :< B,
    string(StartAt),
    atom_string(StartAtKey, StartAt),
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
