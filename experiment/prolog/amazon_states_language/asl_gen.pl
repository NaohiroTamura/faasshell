%% -*- mode: prolog; coding: utf-8; -*-
%%
%% $Id$
%%
%% $ swipl -o exec -g main --stand_alone=true -c asl_validate.pl

:- use_module(library(http/json)).
%:- use_module(library(http/json_convert)).
%:- use_module(library(http/http_json)).

main :-
    open("example.json",read,S,[]),
    json_read_dict(S,Asl,[]),
    close(S),
    Asl = _{'Comment':Comment,'StartAt':StartAt,'States':States},
    writeln(StartAt),
    parse(States,StartAt),
    %
    %json_write_dict(user_output,Asl,[]),
    writeln("Done!").
    %halt.

parse(States,State) :-
    atom_string(Key, State),
    States.Key = _{'Type':"Task",'Resource':Resource,'End':Next},
    writeln(Next).

parse(States,State) :-
    atom_string(Key, State),
    States.Key = _{'Type':"Task",'Resource':Resource,'Next':Next},
    writeln(Next),
    parse(States,Next).

parse(States,State) :-
    atom_string(Key, State),
    States.Key = _{'Type':"Choice",'Choices':Choices,'Default':Default},
    %writeln(Choices),
    choices(States,Choices),
    writeln(Default),
    parse(States,Default).

parse(States,State,Cause) :-
    atom_string(Key, State),
    States.Key = _{'Type':"Fail",'Error':Error,'Cause':Cause},
    writeln(Cause).

choices(States,[]).
choices(States,[C|Cs]) :-
    C.'Next' = Next,
    writeln(Next),
    parse(States,Next),
    choices(States,Cs).
    
