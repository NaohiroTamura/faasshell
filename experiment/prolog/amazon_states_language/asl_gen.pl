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
    catch(main(File, _Dsl, _Graph),
          Err,
          (print_message(error, Err), fail)), !,
    writeln(current_output, ok).

%% swipl -q -l asl_gen.pl -g 'gen_dsl("blueprints/hello_world.json")' -t halt
gen_dsl(File) :-
    catch(main(File, Dsl, _Graph),
          Err,
          (print_message(error, Err), fail)), !,
    format(current_output, '~p.~n', [Dsl]).

%% swipl -q -l asl_gen.pl -g 'gen_dot("blueprints/hello_world.json")' -t halt
gen_dot(File) :-
    catch(main(File, _Dsl, Graph),
          Err,
          (print_message(error, Err), fail)), !,
    graphdot(Graph).

edgedot([]).
edgedot([A>B|Gs]) :-
    format(current_output, '     "~w" -> "~w" ;~n', [A,B]),
    edgedot(Gs).

graphdot(Graph) :- 
    writeln(current_output, "digraph graph_name {"),
    list_to_set(Graph, GraphSet),
    edgedot(GraphSet),
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
    _{'Type':"Pass", 'End':true} :<States.StateKey,
    pass_optional(States.StateKey, Optional),
    Dsl = [pass(StateKey, Optional)],
    Graph = [StateKey>'End'].

parse(States, StateKey, Dsl, Graph, Path) :-
    _{'Type':"Pass",'Next':Next} :< States.StateKey,
    string(Next),
    atom_string(NextKey, Next),
    pass_optional(States.StateKey, Optional),
    parse_next(States, StateKey, NextKey, pass(StateKey, Optional),
               Dsl, Graph, Path).

%% Task State
parse(States, StateKey, Dsl, Graph, Path) :-
    _{'Type':"Task", 'Resource':Resource, 'Next':Next} :< States.StateKey,
    string(Next),
    atom_string(NextKey, Next),
    task_optional(States, StateKey, Optional, G1, Path),
    parse_next(States, StateKey, NextKey, task(StateKey, Resource, Optional),
               Dsl, G2, Path),
    append(G1, G2, Graph).

parse(States, StateKey, Dsl, [StateKey>'End' | Graph], Path) :-
    _{'Type':"Task", 'Resource':Resource, 'End':true} :< States.StateKey,
    task_optional(States, StateKey, Optional, Graph, Path),
    Dsl = [task(StateKey, Resource, Optional)].
    
%% Choice State
parse(States, StateKey, Dsl, Graph, Path) :-
    _{'Type':"Choice", 'Choices':Choices} :< States.StateKey,
    choices(States, StateKey, Choices, D1, G1, Path),
    choice_optional(States, StateKey, Optional, G2, Path),
    Dsl = [choices(StateKey, D1, Optional)],
    append(G1, G2, Graph).

%% Wait State
parse(States, StateKey, Dsl, Graph, Path) :-
    _{'Type':"Wait", 'Next':Next} :< States.StateKey,
    string(Next),
    atom_string(NextKey, Next),
    wait_required(States.StateKey, Wait, Optional),
    parse_next(States, StateKey, NextKey, wait(StateKey, Wait, Optional),
               Dsl, Graph, Path).

%% Should Wait State with End be error?
parse(States, StateKey, [wait(StateKey, Wait, Optional)],
                        [StateKey>'End'], _Path) :-
    _{'Type':"Wait", 'End':true} :< States.StateKey,
    wait_required(States.StateKey, Wait, Optional).

%% Succeed State
parse(States, StateKey,
      [succeed(StateKey, Optional)], [StateKey>'Succeed'], _Path) :-
    _{'Type':"Succeed"} :< States.StateKey,
    common_optional(States.StateKey, Optional).

%% Fail State
parse(States, StateKey, [fail(StateKey, Optional)], [StateKey>'Fail'], _Path) :-
    _{'Type':"Fail"} :< States.StateKey,
    fail_optional(States.StateKey, Optional).

%% Parallel State
parse(States, StateKey, Dsl, Graph, Path) :-
    _{'Type':"Parallel",'Branches':Branches,'Next':Next} :< States.StateKey,
    string(Next), 
    branches(States, StateKey, Branches, D1, G1, [StateKey>NextKey | Path]),
    atom_string(NextKey, Next),
    task_optional(States, StateKey, Optional, G2, Path),
    parse_next(States, StateKey, NextKey,
               parallel(StateKey, branches(D1), Optional), Dsl, G3, Path),
    flatten([G1, G2, G3], Graph).

parse(States, StateKey,  Dsl, Graph, Path) :-
    _{'Type':"Parallel",'Branches':Branches,'End':true} :< States.StateKey,
    branches(States, StateKey, Branches, D1, G1),
    task_optional(States, StateKey, Optional, G2, Path),
    Dsl = [parallel(StateKey, branches(D1), Optional)],
    flatten([[StateKey>'End'], G1, G2], Graph).

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
wait_required(State, Wait, Optional) :-
    ((_{'Seconds': Seconds} :< State, !, Wait = seconds(Seconds));
     (_{'Timestamp': Timestamp} :< State, !, Wait = timestamp(Timestamp));
     (_{'SecondsPath': SecondsPath} :< State, !, Wait = seconds_path(SecondsPath));
     (_{'TimestampPath': TimestampPath} :< State, !,
                                            Wait = timestamp_path(TimestampPath));
     (throw(syntax_error("Wait State doesn't have either Seconds, \c
                                      Timestamp, Timestamp, or TimestampPath")))),
    common_optional(State, Optional).

common_optional(State, Optional) :-
    ( _{'Comment': Comment} :< State, O1 = comment(Comment); O1 = [] ),
    ( _{'InputPath': InputPath} :< State, O2 = input_path(InputPath); O2 = [] ),
    ( _{'OutputPath': OutputPath} :< State, O3 = output_path(OutputPath); O3 = [] ),
    flatten([O1, O2, O3], Optional).

pass_optional(State, Optional) :-
    ( _{'Result': Result} :< State
      -> O1 = result(Result); O1 = [] ),
    ( _{'ResultPath': ResultPath} :< State
      -> O2 = result_path(ResultPath); O2 = [] ),
    common_optional(State, O3),
    flatten([O1, O2, O3], Optional).

task_optional(States, StateKey, Optional, Graph, Path) :-
    ( _{'ResultPath': ResultPath} :< States.StateKey
      -> O1 = result_path(ResultPath); O1 = [] ),
    common_optional(States.StateKey, O2),
    task_fallback(States, StateKey, O3, Graph, Path),
    task_retry(States.StateKey, O4),
    flatten([O1, O2, O3, O4], Optional).

choice_optional(States, StateKey, Optional, Graph, Path) :-
    common_optional(States.StateKey, O1),
    ( _{'Default':Default} :< States.StateKey
      -> string(Default),
         atom_string(DefaultKey, Default),
         parse(States, DefaultKey, Dsl, G1, [StateKey>DefaultKey | Path]),
         Optional = [default(Dsl) | O1],
         Graph = [StateKey>DefaultKey | G1]
      ;  Optional = [O1],
         Graph = []
    ).

fail_optional(State, Optional) :-
    ( _{'Error':Error} :< State -> O1 = error(Error); O1 = [] ),
    ( _{'Cause':Cause} :< State -> O2 = cause(Cause); O2 = [] ),
    common_optional(State, O3),
    flatten([O1, O2, O3], Optional).

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
retry_rules(Rule, 'ErrorEquals'(ErrorCodes, I, M, B)) :-
    _{'ErrorEquals':ErrorCodes} :< Rule,
    ( _{'IntervalSeconds': I} :< Rule; I = 1 ),
    ( _{'MaxAttempts': M} :< Rule; M = 3 ),
    ( _{'BackoffRate': B} :< Rule; B = 2.0 ).

retry_rules_next([], []).
retry_rules_next([R|Rs], [T|Ts]) :-
    retry_rules(R, T),
    retry_rules_next(Rs, Ts).

task_retry(State, Dsl) :-
    _{'Retry': Retriers} :< State
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

:- begin_tests(invalid).

test(abnormal) :-
    main('test/has-dupes.json', _D, _G).

test(abnormal) :-
    main('test/linked-parallel.json', _D, _G).

test(abnormal) :-
    main('test/minimal-fail-state.json', _D, _G).

test(abnormal) :-
    main('test/no-terminal.json', _D, _G).

:- end_tests(invalid).
