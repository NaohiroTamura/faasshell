%% -*- mode: prolog; coding: utf-8; -*-
%%
%% $Id$
%%
%% $ swipl -o exec -g main --stand_alone=true -c asl_validate.pl
%%

:- use_module(library(http/json)).

graphdot([]).
graphdot([A>B|Gs]) :-
    format("     ~w -> ~w ;~n", [A,B]),
    graphdot(Gs).
graphviz(Graph) :- 
    writeln("digraph graph_name {"),
    list_to_set(Graph, GraphSet),
    graphdot(GraphSet),
    writeln("}").

main(File, Dsl, Graph) :-
    load_json(File, Asl),
    parse(Asl, Dsl, Graph).

load_json(File, Asl) :-
    open(File, read, S, []),
    json_read_dict(S, Asl, []),
    close(S).

%% Asl Root
parse(Asl, dsl(Dsl), Graph) :-
    _{'StartAt':StartAt, 'States':States} :< Asl,
    string(StartAt),
    atom_string(StartAtKey, StartAt),
    parse(States, StartAtKey, Dsl, G1),
    Graph = ['Start'>StartAtKey | G1].

%% Pass State
parse(States, StateKey, pass([StateKey, Opts]), [StateKey>'End']) :-
    _{'Type':"Pass", 'End':true} :< States.StateKey,
    del_dict('Type', States.StateKey, _, J1),
    del_dict('End', J1, _, J2),
    atom_json_dict(Opts, J2, []).

parse(States, StateKey, Dsl, Graph) :-
    _{'Type':"Pass",'Next':Next} :< States.StateKey,
    string(Next),
    atom_string(NextKey, Next),
    parse(States, NextKey, D1, G1),
    del_dict('Type', States.StateKey, _, J1),
    del_dict('Next', J1, _, J2),
    atom_json_dict(Opts, J2, []),
    Dsl = [pass([StateKey, Opts]) | D1],
    Graph = [StateKey>NextKey | G1].

%% Task State
parse(States, StateKey, Dsl, Graph) :-
    _{'Type':"Task", 'Resource':Resource, 'Next':Next} :< States.StateKey,
    string(Next),
    atom_string(NextKey, Next),
    parse(States, NextKey, D1, G1),
    Dsl = [task(StateKey, Resource) | D1],
    Graph = [StateKey>NextKey | G1].

parse(States, StateKey, Dsl, [StateKey>'End']) :-
    _{'Type':"Task", 'Resource':Resource, 'End':true} :< States.StateKey,
    (
        _{'Retry': Retriers} :< States.StateKey
        ->  retry_rules_next(Retriers, RetriersTerm),
            Dsl = [task(StateKey, Resource, retry(RetriersTerm))]
        ;   Dsl = [task(StateKey, Resource)]
    ).
    
%% Choice State
parse(States, StateKey, [choices(StateKey, Dsl)], Graph) :-
    _{'Type':"Choice", 'Choices':Choices} :< States.StateKey,
    choices(States, StateKey, Choices, D1, G1),
    (
        _{'Default':Default} :< States.StateKey
        -> string(Default),
           atom_string(DefaultKey, Default),
           parse(States, DefaultKey, D2, G2),
           append(D1, [default(D2)], Dsl),
           append(G1, [StateKey>DefaultKey | G2], Graph)
        ;  Dsl = D1, Graph = G1
    ).

%% Wait State
parse(States, StateKey, Dsl, Graph) :-
    _{'Type':"Wait", 'Next':Next} :< States.StateKey,
    string(Next),
    atom_string(NextKey, Next),
    parse(States, NextKey, D1, G1),
    Dsl = [wait(StateKey) | D1],
    Graph = [StateKey>NextKey | G1].

parse(States, StateKey, [wait(StateKey)], [StateKey>'End']) :-
    _{'Type':"Wait", 'End':true} :< States.StateKey.

%% Succeed State
parse(States, StateKey, [succeed(StateKey)], [StateKey>'Succeed']) :-
    _{'Type':"Succeed"} :< States.StateKey.

%% Fail State
parse(States, StateKey, [fail(StateKey, Error, Cause)], [StateKey>'Fail']) :-
    _{'Type':"Fail"} :< States.StateKey,
    (_{'Error':Error} :< States.StateKey -> true; Error = unknown),
    (_{'Cause':Cause} :< States.StateKey -> true; Cause = unknown).

%% Parallel State
parse(States, StateKey, Dsl, Graph) :-
    _{'Type':"Parallel",'Branches':Branches,'Next':Next} :< States.StateKey,
    string(Next),
    branches(States, StateKey, Branches, D1, G1),
    atom_string(NextKey, Next),
    parse(States, NextKey, D2, G2),
    Dsl = [parallel(StateKey, branches(D1), D2)],
    append(G1, [StateKey>NextKey | G2], Graph).

parse(States, StateKey,  Dsl, Graph) :-
    _{'Type':"Parallel",'Branches':Branches,'End':true} :< States.StateKey,
    branches(States, StateKey, Branches, D1, G1),
    Dls = [parallel(StateKey, branches(D1))],
    Graph = [StateKey>'End' | G1].

%%
choices(_States, _StateKey, [], [], []).
choices(States, StateKey, [C|Cs], Dsl, Graph) :-
    _{'Next':Next} :< C,
    string(Next),
    atom_string(NextKey, Next),
    choice_rules(C, T),
    parse(States, NextKey, D1, G1),
    choices(States, StateKey, Cs, D2, G2),
    Dsl = [case(T, D1) | D2],
    append([StateKey>NextKey | G1], G2, Graph).

%%
branches(_States, _StateKey, [], [], []).
branches(States, StateKey, [B|Bs], [D|Ds], Graph) :-
    _{'StartAt':StartAt,'States':PStates} :< B,
    string(StartAt),
    atom_string(StartAtKey, StartAt),
    parse(PStates, StartAtKey, D, G1),
    branches(States, StateKey, Bs, Ds, G2),
    append([PStates>StartAtKey | G1], G2, Graph).

%%
choice_rules(Rules, 'BooleanEquals'(Variable, Bool)) :-
    _{'Variable': Variable, 'BooleanEquals': Bool} :< Rules.

choice_rules(Rules, 'NumericEquals'(Variable, Num)) :-
    _{'Variable': Variable, 'NumericEquals': Num} :< Rules.

choice_rules(Rules, 'NumericGreaterThan'(Variable, Num)) :-
    _{'Variable': Variable, 'NumericGreaterThan': Num} :< Rules.

choice_rules(Rules, 'NumericGreaterThanEquals'(Variable, Num)) :-
    _{'Variable': Variable, 'NumericGreaterThanEquals': Num} :< Rules.

choice_rules(Rules, 'NumericLessThan'(Variable, Num)) :-
    _{'Variable': Variable, 'NumericLessThan': Num} :< Rules.

choice_rules(Rules, 'NumericLessThanEquals'(Variable, Num)) :-
    _{'Variable': Variable, 'NumericLessThanEquals': Num} :< Rules.

choice_rules(Rules, 'StringEquals'(Variable, Str)) :-
    _{'Variable': Variable, 'StringEquals': Str} :< Rules.

choice_rules(Rules, 'StringGreaterThan'(Variable, Str)) :-
    _{'Variable': Variable, 'StringGreaterThan': Str} :< Rules.

choice_rules(Rules, 'StringGreaterThanEquals'(Variable, Str)) :-
    _{'Variable': Variable, 'StringGreaterThanEquals': Str} :< Rules.

choice_rules(Rules, 'StringLessThan'(Variable, Str)) :-
    _{'Variable': Variable, 'StringLessThan': Str} :< Rules.

choice_rules(Rules, 'StringLessThanEquals'(Variable, Str)) :-
    _{'Variable': Variable, 'StringLessThanEquals': Str} :< Rules.

choice_rules(Rules, 'TimestampEquals'(Variable, Time)) :-
    _{'Variable': Variable, 'TimestampEquals': Time} :< Rules.

choice_rules(Rules, 'TimestampGreaterThan'(Variable, Time)) :-
    _{'Variable': Variable, 'TimestampGreaterThan': Time} :< Rules.

choice_rules(Rules, 'TimestampGreaterThanEquals'(Variable, Time)) :-
    _{'Variable': Variable, 'TimestampGreaterThanEquals': Time} :< Rules.

choice_rules(Rules, 'TimestampLessThan'(Variable, Time)) :-
    _{'Variable': Variable, 'TimestampLessThan': Time} :< Rules.

choice_rules(Rules, 'TimestampLessThanEquals'(Variable, Time)) :-
    _{'Variable': Variable, 'TimestampLessThanEquals': Time} :< Rules.

%% the value of a Not operator must be a single Choice Rule
%% that must not contain Next fields. 
choice_rules(Rules, 'Not'(Term)) :-
    _{'Not': Cond} :< Rules,
    choice_rules(Cond, Term).

choice_rules(Rules, 'And'(Terms)) :-
    _{'And': Conds} :< Rules,
    choice_rules_next(Conds, Terms).

choice_rules(Rules, 'Or'(Terms)) :-
    _{'Or': Conds} :< Rules,
    choice_rules_next(Conds, Terms).
%%
choice_rules_next([], []).
choice_rules_next([C|Cs], [T|Ts]) :-
    choice_rules(C, T),
    choice_rules_next(Cs, Ts).

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

test(hello) :- main('blueprints/hello_world.json', D, G).

test(choice) :- main('blueprints/choice_state.json', D, G).

test(choice) :- main('blueprints/choice_statex.json', D, G).

test(choice) :- main('blueprints/catch_failure.json', D, G).

test(choice) :- main('blueprints/job_status_poller.json', D, G).

test(choice) :- main('blueprints/parallel.json', D, G).

test(choice) :- main('blueprints/retry_failure.json', D, G).

test(choice) :- main('blueprints/task_timer.json', D, G).

test(choice) :- main('blueprints/wait_state.json', D, G).

:- end_tests(blueprints).

:- begin_tests(actions).
%test(abnormal) :- main('test/has-dupes.json').
%test(abnormal) :- main('test/linked-parallel.json').
%test(abnormal) :- main('test/minimal-fail-state.json').
%test(abnormal) :- main('test/no-terminal.json').
:- end_tests(actions).
