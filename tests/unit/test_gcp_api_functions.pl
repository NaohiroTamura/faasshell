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

:- include('../../src/gcp_api_functions.pl').

%%
%% Unit Tests
%%
:- use_module(library(plunit)).


personal(Location, Project) :-
    getenv('gcp_location_id', Location),
    getenv('gcp_project_id', Project).


:- begin_tests(list).

test(all, Code = 200) :-
    gcp_api_functions:faas:list([], [status_code(Code)], R),
    assertion(is_list(R)).

test(hello, Code = 200) :-
    personal(Location, Project),
    atomic_list_concat([frn, gcp, functions, Location, Project, function, hello],
                       ':', FRN),
    gcp_api_functions:faas:list(FRN, [status_code(Code)], R),
    atomics_to_string([projects, Project, locations, Location, functions, hello],
                      '/', Name),
    assertion(Name = R.name).

:- end_tests(list).

:- begin_tests(invoke).

test(hello_noarg, (Code, R) = (200, _{payload:"Hello, World!"})) :-
    personal(Location, Project),
    atomic_list_concat([frn, gcp, functions, Location, Project, function, hello],
                       ':', FRN),
    gcp_api_functions:faas:invoke(FRN, [status_code(Code)], _{}, R).

test(hello_arg, (Code, R) = (200, _{payload:"Hello, GCP!"})) :-
    personal(Location, Project),
    atomic_list_concat([frn, gcp, functions, Location, Project, function, hello],
                       ':', FRN),
    gcp_api_functions:faas:invoke(FRN, [status_code(Code)], _{name: "GCP"}, R).

%% should not be error such as AWS, Azure
test(hello_badarg, (Code, ErrorMsg) = (400, 'Bad Request')) :-
    personal(Location, Project),
    atomic_list_concat([frn, gcp, functions, Location, Project, function, hello],
                       ':', FRN),
    catch(gcp_api_functions:faas:invoke(FRN, [status_code(Code)], '', _R),
          Error,
          true),
    _{cause:status_code(Code), error: ErrorHtml } :< Error,
    %% Error.error changed string to html
    %% '<!DOCTYPE html>\n<html lang="en">\n<head>\n<meta charset="utf-8">\n<title>Error</title>\n</head>\n<body>\n<pre>Bad Request</pre>\n</body>\n</html>\n'
    setup_call_cleanup(
            open_string(ErrorHtml, S),
            load_html(S, ErrorDom , []),
            close(S)),
    xpath(ErrorDom, //(pre(normalize_space)), ErrorMsg).

:- end_tests(invoke).
