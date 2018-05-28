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

:- module(gcp_api_functions,
          [ faas:list/3,
            faas:invoke/4
         ]).

:- use_module(gcp_api_utils).
:- use_module(json_utils).
:- use_module(proxy_utils).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).


:- multifile
       faas:list/3,
       faas:invoke/4.

faas:list([], Options, Reply) :-
    gcp_api_utils:gcp(GcpOptions),
    merge_options(Options, GcpOptions, MergedOptions),
    option(gcp_project(Project), GcpOptions),
    atomic_list_concat(['https://cloudfunctions.googleapis.com/v1/projects/',
                        Project,
                        '/locations/-/functions'], URL),
    http_get(URL, R1, MergedOptions),
    json_utils:term_json_dict(R1, R2),
    get_dict('functions', R2, Reply).

faas:list(FRN, Options, Reply) :-
    atom(FRN), !,
    atomic_list_concat([frn, gcp, functions, Location, Project, function, Function],
                       ':', FRN),
    gcp_api_utils:gcp(GcpOptions),
    merge_options(Options, GcpOptions, MergedOptions),
    atomic_list_concat(['https://cloudfunctions.googleapis.com/v1/projects/',
                        Project,
                        '/locations/',
                        Location,
                        '/functions/',
                       Function], URL),
    http_get(URL, R1, MergedOptions),
    json_utils:term_json_dict(R1, Reply).


faas:invoke(FRN, Options, Payload, Reply) :-
    atomic_list_concat([frn, gcp, functions, Location, Project, function, Function],
                       ':', FRN), !,
    atomic_list_concat(['https://', Location, '-', Project, '.cloudfunctions.net/',
                        Function], URL),
    proxy_utils:http_proxy(URL, ProxyOptions),
    GcpOptions = [ status_code(_) %% , cert_verify_hook(cert_accept_any)
                   | ProxyOptions],
    merge_options(Options, GcpOptions, MergedOptions),
    json_utils:term_json_dict(Json, Payload),
    http_post(URL, json(Json), R1, MergedOptions),
    option(status_code(Code), MergedOptions),
    ( Code = 200
      -> json_utils:term_json_dict(R1, Reply)
      ;  Reply = _{error: R1, cause: status_code(Code)},
         throw(Reply)
    ).
