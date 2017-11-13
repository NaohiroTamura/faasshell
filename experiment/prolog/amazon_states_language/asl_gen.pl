%% -*- mode: prolog; coding: utf-8; -*-
%%
%% $Id$
%%
%% Amazon State Language (ALS) Parser, DSL and Graph Generator
%%

:- module(asl_gen,
          [ validate/1,
            gen_dsl/1,
            gen_dot/1
         ]).

:- use_module(library(http/json)).

%% swipl -q -l asl_gen.pl -g 'validate("blueprints/hello_world.json")' -t halt
validate(File) :-
    main(File, _Dsl, _Graph),
    writeln(current_output, ok).

%% swipl -q -l asl_gen.pl -g 'gen_dsl("blueprints/hello_world.json")' -t halt
gen_dsl(File) :-
    main(File, Dsl, _Graph),
    format(current_output, '~p.~n', [Dsl]).

%% swipl -q -l asl_gen.pl -g 'gen_dot("blueprints/hello_world.json")' -t halt
gen_dot(File) :-
    main(File, _Dsl, Graph),
    graphdot(Graph).

graphdot([]).
graphdot([A>B|Gs]) :-
    format(current_output, '     "~w" -> "~w" ;~n', [A,B]),
    graphdot(Gs).

graphdot(Graph) :- 
    writeln(current_output, "digraph graph_name {"),
    list_to_set(Graph, GraphSet),
    graphdot(GraphSet),
    writeln(current_output, "}").

%%
main(File, Dsl, Graph) :-
    load_json(File, Asl),
    parse(Asl, Dsl, Graph, []).

load_json(File, Asl) :-
    open(File, read, S, []),
    json_read_dict(S, Asl, []),
    close(S).

%% Asl Root
parse(Asl, asl(Dsl), Graph, Path) :-
    _{'StartAt':StartAt, 'States':States} :< Asl,
    string(StartAt),
    atom_string(StartAtKey, StartAt),
    parse(States, StartAtKey, Dsl, G1, ['Start'>StartAtKey | Path]),
    Graph = ['Start'>StartAtKey | G1].

%% Pass State
parse(States, StateKey, Dsl, Graph, _Path) :-
    select_dict(_{'Type':"Pass", 'End':true}, States.StateKey, Rest),
    atom_json_dict(Opts, Rest, []),
    Dsl = [pass(StateKey, Opts)],
    Graph = [StateKey>'End'].

parse(States, StateKey, Dsl, Graph, Path) :-
    select_dict(_{'Type':"Pass",'Next':Next}, States.StateKey, Rest),
    string(Next),
    atom_string(NextKey, Next),
    atom_json_dict(Opts, Rest, []),
    parse_next(States, StateKey, NextKey, pass(StateKey, Opts), Dsl, Graph, Path).

%% Task State
parse(States, StateKey, Dsl, Graph, Path) :-
    _{'Type':"Task", 'Resource':Resource, 'Next':Next} :< States.StateKey,
    string(Next),
    atom_string(NextKey, Next),
    parse_next(States, StateKey, NextKey, task(StateKey, Resource),
               Dsl, Graph, Path).

parse(States, StateKey, [Dsl], [StateKey>'End' | G1], Path) :-
    _{'Type':"Task", 'Resource':Resource, 'End':true} :< States.StateKey,
    task_fallback(States, StateKey, D1, G1, Path),
    task_retry(States, StateKey, D2),
    append(D1, D2, D3),
    Dsl =.. [task, StateKey, Resource | D3].
    
%% Choice State
parse(States, StateKey, [choices(StateKey, Dsl)], Graph, Path) :-
    _{'Type':"Choice", 'Choices':Choices} :< States.StateKey,
    choices(States, StateKey, Choices, D1, G1, Path),
    (
        _{'Default':Default} :< States.StateKey
        -> string(Default),
           atom_string(DefaultKey, Default),
           parse(States, DefaultKey, D2, G2, [StateKey>DefaultKey | Path]),
           append(D1, [default(D2)], Dsl),
           append(G1, [StateKey>DefaultKey | G2], Graph)
        ;  Dsl = D1, Graph = G1
    ).

%% Wait State
parse(States, StateKey, Dsl, Graph, Path) :-
    _{'Type':"Wait", 'Next':Next} :< States.StateKey,
    string(Next),
    atom_string(NextKey, Next),
    parse_next(States, StateKey, NextKey, wait(StateKey), Dsl, Graph, Path).

parse(States, StateKey, [wait(StateKey)], [StateKey>'End'], _Path) :-
    _{'Type':"Wait", 'End':true} :< States.StateKey.

%% Succeed State
parse(States, StateKey, [succeed(StateKey)], [StateKey>'Succeed'], _Path) :-
    _{'Type':"Succeed"} :< States.StateKey.

%% Fail State
parse(States, StateKey, [fail(StateKey, Error, Cause)], [StateKey>'Fail'], _Path) :-
    _{'Type':"Fail"} :< States.StateKey,
    (_{'Error':Error} :< States.StateKey -> true; Error = unknown),
    (_{'Cause':Cause} :< States.StateKey -> true; Cause = unknown).

%% Parallel State
parse(States, StateKey, Dsl, Graph, Path) :-
    _{'Type':"Parallel",'Branches':Branches,'Next':Next} :< States.StateKey,
    string(Next), 
    branches(States, StateKey, Branches, D1, G1, [StateKey>NextKey | Path]),
    atom_string(NextKey, Next),
    parse(States, NextKey, D2, G2, [StateKey>NextKey |Path]),
    Dsl = [parallel(StateKey, branches(D1), D2)],
    append(G1, [StateKey>NextKey | G2], Graph).

parse(States, StateKey,  Dsl, Graph, _Path) :-
    _{'Type':"Parallel",'Branches':Branches,'End':true} :< States.StateKey,
    branches(States, StateKey, Branches, D1, G1),
    Dsl = [parallel(StateKey, branches(D1))],
    Graph = [StateKey>'End' | G1].

%%
%% parse next unless cycled
parse_next(States, StateKey, NextKey, Term, Dsl, Graph, Path) :-
    (
        memberchk(StateKey>NextKey, Path)
     -> Dsl = [Term],
        Graph = [StateKey>NextKey]
     ;  parse(States, NextKey, D1, G1, [StateKey>NextKey | Path]),
        Dsl = [Term | D1],
        Graph = [StateKey>NextKey | G1]
    ).

%%
choices(_States, _StateKey, [], [], [], _Path).
choices(States, StateKey, [C|Cs], Dsl, Graph, Path) :-
    _{'Next':Next} :< C,
    string(Next),
    atom_string(NextKey, Next),
    choice_rules(C, T),
    parse(States, NextKey, D1, G1, [StateKey>NextKey | Path]),
    choices(States, StateKey, Cs, D2, G2, Path),
    Dsl = [case(T, D1) | D2],
    append([StateKey>NextKey | G1], G2, Graph).

%%
branches(_States, _StateKey, [], [], [], _Path).
branches(States, StateKey, [B|Bs], [D|Ds], Graph, Path) :-
    _{'StartAt':StartAt,'States':PStates} :< B,
    string(StartAt),
    atom_string(StartAtKey, StartAt),
    parse(PStates, StartAtKey, D, G1, [StateKey>StartAtKey | Path]),
    branches(States, StateKey, Bs, Ds, G2, Path),
    append([StateKey>StartAtKey | G1], G2, Graph).

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
retry_rules(Rule, 'ErrorEquals'(ErrorCodes)) :-
    _{'ErrorEquals':ErrorCodes} :< Rule.

retry_rules_next([], []).
retry_rules_next([R|Rs], [T|Ts]) :-
    retry_rules(R, T),
    retry_rules_next(Rs, Ts).

task_retry(States, StateKey, Dsl) :-
    _{'Retry': Retriers} :< States.StateKey
    ->  retry_rules_next(Retriers, RetriersTerm),
            Dsl = [retry(RetriersTerm)]
        ;   Dsl = [].

%% 
fallback_rules(States, StateKey, Rule, Dsl, Graph, Path) :-
    _{'ErrorEquals':ErrorCodes, 'Next':Next} :< Rule,
    string(Next),
    atom_string(NextKey, Next),
    parse(States, NextKey, D1, G1, [StateKey>NextKey | Path]),
    Dsl = [case('ErrorEquals'(ErrorCodes), D1)],
    Graph = [StateKey>NextKey | G1].

fallback_rules_next(_, _, [], [], [], _).
fallback_rules_next(States, StateKey, [R|Rs], Dsl, Graph, Path) :-
    fallback_rules(States, StateKey, R, D1, G1, Path),
    fallback_rules_next(States, StateKey, Rs, D2, G2, Path),
    append(D1, D2, Dsl),
    append(G1, G2, Graph).

task_fallback(States, StateKey, Dsl, Graph, Path) :-
    _{'Catch': Catchers} :< States.StateKey
    ->  fallback_rules_next(States, StateKey, Catchers, D2, G2, Path),
            Dsl = [fallback(D2)], Graph = G2
        ;   Dsl = [], Graph = [].

%%
%% Unit Tests
%%
:- use_module(library(plunit)).

:- begin_tests(blueprints).

test(hello) :- main('blueprints/hello_world.json', _D, _G).

test(choice) :- main('blueprints/choice_state.json', _D, _G).

test(choicex) :- main('blueprints/choice_statex.json', _D, _G).

test(catch) :- main('blueprints/catch_failure.json', _D, _G).

test(poller) :- main('blueprints/job_status_poller.json', _D, _G).

test(parallel) :- main('blueprints/parallel.json', _D, _G).

test(retry) :- main('blueprints/retry_failure.json', _D, _G).

test(timer) :- main('blueprints/task_timer.json', _D, _G).

test(wait) :- main('blueprints/wait_state.json', _D, _G).

:- end_tests(blueprints).

:- begin_tests(actions).
%test(abnormal) :- main('test/has-dupes.json').
%test(abnormal) :- main('test/linked-parallel.json').
%test(abnormal) :- main('test/minimal-fail-state.json').
%test(abnormal) :- main('test/no-terminal.json').
:- end_tests(actions).
