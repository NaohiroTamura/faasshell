%% -*- mode: prolog; coding: utf-8; -*-
%%
%% $Id$
%%
%% $ swipl -o exec -g main --stand_alone=true -c asl_validate.pl
%%

:- use_module(library(http/json)).

gencode(Code) :- format("~w~n", [Code]).
gencode(Code, next) :- format("~p,~n", [Code]).
gencode(Code, end) :- format("~p~n", [Code]).

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

%% Asl Root
parse(Asl, Graph) :-
    _{'StartAt':StartAt, 'States':States} :< Asl,
    string(StartAt),
    atom_string(StartAtKey, StartAt),
    gencode( 'dsl([' ),
    parse(States, StartAtKey, G1),
    Graph = ['Start'>StartAtKey | G1],
    gencode( ']) /* dsl */' ).

%% Pass State
parse(States, StateKey, [StateKey>'End']) :-
    _{'Type':"Pass", 'End':true} :< States.StateKey,
    del_dict('Type', States.StateKey, _, J1),
    del_dict('End', J1, _, J2),
    atom_json_dict(J4, J2, []),
    gencode(pass([StateKey, J4])).

parse(States, StateKey, Graph) :-
    _{'Type':"Pass",'Next':Next} :< States.StateKey,
    string(Next),
    atom_string(NextKey, Next),
    gencode(pass(StateKey)),
    parse(States, NextKey, G1),
    Graph = [StateKey>NextKey | G1].

%% Task State
parse(States, StateKey, Graph) :-
    _{'Type':"Task", 'Resource':Resource, 'Next':Next} :< States.StateKey,
    string(Next),
    atom_string(NextKey, Next),
    gencode(task(StateKey, Resource), next),
    parse(States, NextKey, G1),
    Graph = [StateKey>NextKey | G1].

parse(States, StateKey, [StateKey>'End']) :-
    _{'Type':"Task", 'Resource':Resource, 'End':true} :< States.StateKey,
    (
        _{'Retry': Retriers} :< States.StateKey
        ->  retry_rules_next(Retriers, RetriersTerm),
            gencode(task(StateKey, Resource, retry(RetriersTerm)), end)
        ;   gencode(task(StateKey, Resource), end)
    ).
    

%% Choice State
parse(States, StateKey, Graph) :-
    _{'Type':"Choice", 'Choices':Choices} :< States.StateKey,
    gencode( 'choices(' ),
    gencode( StateKey, next ),
    gencode( '[' ),
    choices(States, StateKey, Choices, G1),
    (  _{'Default':Default} :< States.StateKey
    -> string(Default),
       atom_string(DefaultKey, Default),
       gencode( 'default(' ),
       parse(States, DefaultKey, G2),
       gencode( ')'),
       append(G1, [StateKey>DefaultKey | G2], Graph)
     ; Graph = G1),
    gencode( ']) /* choices */' ).

%% Wait State
parse(States, StateKey, Graph) :-
    _{'Type':"Wait", 'Next':Next} :< States.StateKey,
    string(Next),
    atom_string(NextKey, Next),
    gencode(wait(StateKey)),
    parse(States, NextKey, G1),
    Graph = [StateKey>NextKey | G1].

%% Succeed State
parse(States, StateKey, [StateKey>'Succeed']) :-
    _{'Type':"Succeed"} :< States.StateKey,
    gencode(succeed(StateKey)).

%% Fail State
parse(States, StateKey, [StateKey>'Fail']) :-
    _{'Type':"Fail"} :< States.StateKey,
    (_{'Error':Error} :< States.StateKey -> true; Error = unknown),
    (_{'Cause':Cause} :< States.StateKey -> true; Cause = unknown),
    gencode(fail(StateKey, Error, Cause), end).

%% Parallel State
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
    _{'Type':"Parallel",'Branches':Branches,'End':true} :< States.StateKey,
    gencode(parallel(StateKey)),
    gencode( 'branches( ' ),
    branches(States, StateKey, Branches, G1),
    gencode( ' 7) ' ),
    Graph = [StateKey>'End' | G1],
    gencode( ' 8) ' ).

choices(_States, _StateKey, [], []).
choices(States, StateKey, [C|Cs], Graph) :-
    _{'Next':Next} :< C,
    string(Next),
    atom_string(NextKey, Next),
    gencode( 'case(' ),
    choice_rules(C),
    gencode( ',' ),
    parse(States, NextKey, G1),
    gencode( '), /* case */' ),
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
choice_rules(Rules) :-
    _{'Variable': Variable, 'BooleanEquals': Bool} :< Rules,
    gencode( 'BooleanEquals'(Variable, Bool) ).

choice_rules(Rules) :-
    _{'Variable': Variable, 'NumericEquals': Num} :< Rules,
    gencode( 'NumericEquals'(Variable, Num) ).

choice_rules(Rules) :-
    _{'Variable': Variable, 'NumericGreaterThan': Num} :< Rules,
    gencode( 'NumericGreaterThan'(Variable, Num) ).

choice_rules(Rules) :-
    _{'Variable': Variable, 'NumericGreaterThanEquals': Num} :< Rules,
    gencode( 'NumericGreaterThanEquals'(Variable, Num) ).

choice_rules(Rules) :-
    _{'Variable': Variable, 'NumericLessThan': Num} :< Rules,
    gencode( 'NumericLessThan'(Variable, Num) ).

choice_rules(Rules) :-
    _{'Variable': Variable, 'NumericLessThanEquals': Num} :< Rules,
    gencode( 'NumericLessThanEquals'(Variable, Num) ).

choice_rules(Rules) :-
    _{'Variable': Variable, 'StringEquals': Str} :< Rules,
    gencode( 'StringEquals'(Variable, Str) ).

choice_rules(Rules) :-
    _{'Variable': Variable, 'StringGreaterThan': Str} :< Rules,
    gencode( 'StringGreaterThan'(Variable, Str) ).

choice_rules(Rules) :-
    _{'Variable': Variable, 'StringGreaterThanEquals': Str} :< Rules,
    gencode( 'StringGreaterThanEquals'(Variable, Str) ).

choice_rules(Rules) :-
    _{'Variable': Variable, 'StringLessThan': Str} :< Rules,
    gencode( 'StringLessThan'(Variable, Str) ).

choice_rules(Rules) :-
    _{'Variable': Variable, 'StringLessThanEquals': Str} :< Rules,
    gencode( 'StringLessThanEquals'(Variable, Str) ).

choice_rules(Rules) :-
    _{'Variable': Variable, 'TimestampEquals': Time} :< Rules,
    gencode( 'TimestampEquals'(Variable, Time), next ).

choice_rules(Rules) :-
    _{'Variable': Variable, 'TimestampGreaterThan': Time} :< Rules,
    gencode( 'TimestampGreaterThan'(Variable, Time), next ).

choice_rules(Rules) :-
    _{'Variable': Variable, 'TimestampGreaterThanEquals': Time} :< Rules,
    gencode( 'TimestampGreaterThanEquals'(Variable, Time), next ).

choice_rules(Rules) :-
    _{'Variable': Variable, 'TimestampLessThan': Time} :< Rules,
    gencode( 'TimestampLessThan'(Variable, Time) ).

choice_rules(Rules) :-
    _{'Variable': Variable, 'TimestampLessThanEquals': Time} :< Rules,
    gencode( 'TimestampLessThanEquals'(Variable, Time) ).

%% the value of a Not operator must be a single Choice Rule
%% that must not contain Next fields. 
choice_rules(Rules) :-
    _{'Not': Cond} :< Rules,
    gencode( "'Not'(" ),
    choice_rules(Cond),
    gencode( ')' ).

choice_rules(Rules) :-
    _{'And': Conds} :< Rules,
    gencode( "'And'("),
    choice_rules_next(Conds),
    gencode( ') /* And */' ).

choice_rules(Rules) :-
    _{'Or': Conds} :< Rules,
    gencode( "'Or'(" ),
    choice_rules_next(Conds),
    gencode( ') /* Or */' ).
%%
choice_rules_next([C]) :-
    choice_rules(C), !.

choice_rules_next([C|Cs]) :-
    choice_rules(C),
    gencode( ',' ),
    choice_rules_next(Cs).

%%
retry_rules(R, 'ErrorEquals'(ErrorCodes)) :-
    _{'ErrorEquals': ErrorCodes} :< R.

retry_rules_next([], []).
retry_rules_next([R|Rs], [T|Ts]) :-
    retry_rules(R, T),
    retry_rules_next(Rs, Ts).
    
%%
%% Unit Tests
%%
:- use_module(library(plunit)).

:- begin_tests(blueprints).

test(hello) :- main('blueprints/hello_world.json',_G).

test(choice) :- main('blueprints/choice_state.json',_G).

test(choice) :- main('blueprints/choice_statex.json',_G).

test(choice) :- main('blueprints/catch_failure.json',_G).

test(choice) :- main('blueprints/job_status_poller.json',_G).

test(choice) :- main('blueprints/parallel.json',_G).

test(choice) :- main('blueprints/retry_failure.json',_G).

test(choice) :- main('blueprints/task_timer.json',_G).

test(choice) :- main('blueprints/wait_state.json',_G).

:- end_tests(blueprints).

:- begin_tests(actions).
%test(abnormal) :- main('test/has-dupes.json').
%test(abnormal) :- main('test/linked-parallel.json').
%test(abnormal) :- main('test/minimal-fail-state.json').
%test(abnormal) :- main('test/no-terminal.json').
:- end_tests(actions).
