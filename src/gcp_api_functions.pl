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
          [ %%faas:list/3,
            faas:invoke/4
         ]).

:- use_module(json_utils).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).


:- multifile
       %%faas:list/3,
       faas:invoke/4.

%%faas:list([], Options, Reply) :-

%%faas:list(GRN, Options, Reply) :-

faas:invoke(GRN, Options, Payload, Reply) :-
    atomic_list_concat([grn, gcp, lambda, Region, Project, Domain, Function],
                       ':', GRN), !,
    GcpOptions = [status_code(Code), cert_verify_hook(cert_accept_any)],
    merge_options(GcpOptions, Options, MergedOptions),
    json_utils:term_json_dict(Json, Payload),
    atomic_list_concat(['https://', Region, '-', Project, '.', Domain,
                        '/', Function], URL),
    http_post(URL, json(Json), R1, MergedOptions),
    ( Code = 200
      -> json_utils:term_json_dict(R1, Reply)
      ;  Reply = _{error: R1, cause: status_code(Code)}
    ).
