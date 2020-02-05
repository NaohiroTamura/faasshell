%% -*- mode: prolog; coding: utf-8; -*-
%%
%% Copyright 2017-2020 FUJITSU LIMITED
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

module(graphql).

:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).

:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).

:- use_module(library(dcg/basics)).
:- use_module(library(quasi_quotations)).

:- quasi_quotation_syntax(graphql).

%%
%% GraphQL Quasi Quotations
%%
tokenize(_QQDict, []) -->
    eos,
    %{ writeln(eos) },
    !.
tokenize(QQDict, [' '|Tokens]) -->
    blank, blanks,
    %{ writeln(blanks) },
    !, tokenize(QQDict, Tokens).
tokenize(QQDict, [Value|Tokens]) -->
    prolog_var_name(Key),
    %{ writeln(prolog_var_name(Key, Value)) },
    { ( memberchk(Key=Value, QQDict)
        ; Value = Key ) },
    !, tokenize(QQDict, Tokens).
tokenize(QQDict, ['"',Atom,'"'|Tokens]) -->
    `\"`,
     upto_quote(Atom),
    %{ writeln(upto_quote(Atom)) },
    !, tokenize(QQDict, Tokens).
tokenize(QQDict, [Atom, Sep|Tokens]) -->
    upto_sep(Atom, Sep),
    %{ writeln(upto_sep(Atom, Sep)) },
    !, tokenize(QQDict, Tokens).

upto_sep(Atom, Sep) -->
        string(Codes), [S],
        { atom_codes(Atom, Codes),
          ( code_type(S, space)
            ; code_type(S, prolog_symbol)
            ; code_type(S, paren(_))
            ; code_type(_, paren(S))
            ; code_type(S, end_of_line)
          ),
          !,
          ( code_type(S, end_of_line)
            -> Sep = ''
            ;  atom_codes(Sep, [S])
          )
        }.

upto_quote(Atom) -->
    string(Codes), `\"`, !,
    { atom_codes(Atom, Codes) }.

%%
graphql(Content, Vars, Dict, List) :-
    % format(user_error, 'graphql(~p, ~p, ~p, ~p)~n', [Content, Vars, Dict, List]),

    include(qq_var(Vars), Dict, QQDict),

    % format(user_error, 'qq_var(~p), ~p, ~p))~n', [Vars, Dict, QQDict]),

    phrase_from_quasi_quotation(tokenize(QQDict, List), Content).

qq_var(Vars, _=Var) :- member(V, Vars), V == Var, !.

%%
%% Utils
%%
term_json_dict(Term, Dict) :-
    ground(Term), !,
    atom_json_term(Atom, Term, []), atom_json_dict(Atom, Dict, []).
term_json_dict(Term, Dict) :-
    atom_json_dict(Atom, Dict, []), atom_json_term(Atom, Term, []).

graphql_url('https://api.github.com/graphql').

%%
%% OpenWhisk Action
%%
first_query(Param, Out) :-
    _{ github_token: GITHUB_ACCESS_TOKEN,
       owner: Owner,
       name: Name,
       since: Since,
       until: Until
     } :< Param,

    QueryList =
        {| graphql(Owner, Name, Since, Until) ||
           {
             repository(owner: Owner, name: Name) {
               ref(qualifiedName: "master") {
                 target {
                   ... on Commit {
                     history(first: 100, since: Since, until: Until) {
                       totalCount
                       edges {
                         node {
                           oid
                           messageHeadline
                           author {
                             date
                             email
                           }
                         }
                         cursor
                       }
                       pageInfo {
                         endCursor
                         hasNextPage
                       }
                     }
                   }
                 }
               }
             }
           }
        |},
    atomic_list_concat(QueryList, Query),
    % format('~p~n', [Query]),

    term_json_dict(Json, _{ query: Query }),
    graphql_url(URL),
    atomic_list_concat(['Bearer', GITHUB_ACCESS_TOKEN], ' ', AuthorizationHeader),
    http_post(URL, json(Json), R1,
              [request_header('Authorization'=AuthorizationHeader)]),
    term_json_dict(R1, D1),
    % format('~p~n', [D1]),

    ( get_dict(errors, D1, [Error|_])
      -> % format('errors = ~p~n', [Error.message]),
         throw(Error.message)
      ;  History = D1.data.repository.ref.target.history,
         ( History.pageInfo.hasNextPage
           -> atomic_list_concat(['"', History.pageInfo.endCursor, '"'], After),
              next_query(Param, After, O1),
              append(History.edges, O1, Out)
           ;  Out = History.edges
         )
    ).

next_query(Param, After, Out) :-
    _{ github_token: GITHUB_ACCESS_TOKEN,
       owner: Owner,
       name: Name,
       since: Since,
       until: Until
     } :< Param,

    QueryList =
        {| graphql(Owner, Name, Since, Until, After) ||
           {
             repository(owner: Owner, name: Name) {
               ref(qualifiedName: "master") {
                 target {
                   ... on Commit {
                     history(first: 100, since: Since, until: Until, after: After) {
                       totalCount
                       edges {
                         node {
                           oid
                           messageHeadline
                           author {
                             date
                             email
                           }
                         }
                         cursor
                       }
                       pageInfo {
                         endCursor
                         hasNextPage
                       }
                     }
                   }
                 }
               }
             }
           }
        |},
    atomic_list_concat(QueryList, Query),
    % format('~p~n', [Query]),

    term_json_dict(Json, _{ query: Query }),
    graphql_url(URL),
    atomic_list_concat(['Bearer', GITHUB_ACCESS_TOKEN], ' ', AuthorizationHeader),
    http_post(URL, json(Json), R1,
              [request_header('Authorization'=AuthorizationHeader)]),
    term_json_dict(R1, D1),
    % format('~p~n', [D1]),

    History = D1.data.repository.ref.target.history,
    ( History.pageInfo.hasNextPage
      -> atomic_list_concat(['"', History.pageInfo.endCursor, '"'], A1),
         next_query(Param, A1, O1),
         append(History.edges, O1, Out)
      ;  Out = History.edges
    ).

filter(Target, Elm) :-
    ( Target = ""
      -> true
      ;  sub_string(Elm.node.author.email, _, _, _, Target)
    ),
    %% git log --no-merges
    \+ sub_string(Elm.node.messageHeadline, 0, _, _, "Merge ").

main(In, _{ values: [Values] }) :-
    format(user_output, '~nIn = ~p~n', [In]),
    _{ target: Target, owner: Owner, name: Name, since: Since, until: Until } :< In,
    format(atom(Owner2), '"~w"', [Owner]),
    format(atom(Name2), '"~w"', [Name]),
    ( parse_time(Since, iso_8601, _)
      -> format(atom(Since2), '"~w"', [Since])
      ;  format_time(atom(Since2), '"%FT%T%:z"', 0)
    ),
    ( parse_time(Until, iso_8601, _)
      -> format(atom(Until2), '"~w"', [Until])
      ;  get_time(Now),
         format_time(atom(Until2), '"%FT%T%:z"', Now)
    ),
    Param = In.put(_{ owner: Owner2, name: Name2, since: Since2, until: Until2 }),
    format(user_output, '~nParam = ~p~n', [Param]),
    catch( (first_query(Param, NodeOut),
            include(filter(Target), NodeOut, MergeOut),
            length(MergeOut, Count),
            Values = [Target, Owner2, Name2, Since2, Until2, Count]
           ),
           Errors,
           Values = [Target, Owner2, Name2, Since2, Until2, 0, Errors]
         ).

%%
%% Tests
%%
/*
   $ wsk action create graphql graphql.pl \
     --docker ${docker_image_prefix}/swipl8action -i
*/

/*
   $ wsk action invoke graphql -ir \
     -p target fujitsu.com \
     -p github_token $GITHUB_TOKEN \
     -p owner "naohirotamura" \
     -p name "faasshell" \
     -p since "2018-06-21T00:00:00+00:00" \
     -p until "2018-07-20T00:00:00+00:00"
*/

test1(Out) :-
    getenv('GITHUB_TOKEN', GITHUB_TOKEN),
    main(_{ target: "fujitsu.com",
            github_token: GITHUB_TOKEN,
            owner: "naohirotamura",
            name: "faasshell",
            since: "2018-06-21T00:00:00+00:00",
            until: "2018-07-20T00:00:00+00:00"
          }, Out).

test2(Out) :-
    getenv('GITHUB_TOKEN', GITHUB_TOKEN),
    main(_{ target: "fujitsu.com",
            github_token: GITHUB_TOKEN,
            owner: "containers",
            name: "buildah",
            since: "2018-03-21T00:00:00+00:00",
            until: "2018-04-20T00:00:00+00:00"
          }, Out).

test3(Out) :-
    getenv('GITHUB_TOKEN', GITHUB_TOKEN),
    main(_{ target: '',
            github_token: GITHUB_TOKEN,
            owner: 'naohirotamura',
            name: 'faasshell',
            since: '',
            until: ''
          }, Out).

test4(Out) :-
    getenv('GITHUB_TOKEN', GITHUB_TOKEN),
    main(_{ target: '',
            github_token: GITHUB_TOKEN,
            owner: '',
            name: 'faasshell',
            since: '',
            until: ''
          }, Out).

test5(Out) :-
    getenv('GITHUB_TOKEN', GITHUB_TOKEN),
    main(_{ target: '',
            github_token: GITHUB_TOKEN,
            owner: 'naohirotamura',
            name: '',
            since: '',
            until: ''
          }, Out).
