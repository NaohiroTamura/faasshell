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

:- module(azure_api_functions,
          [ %%faas:list/3,
            faas:invoke/4
         ]).

:- use_module(json_utils).
:- use_module(proxy_utils).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).


:- multifile
       %%faas:list/3,
       faas:invoke/4.

%%faas:list([], Options, Reply) :-

%%faas:list(MRN, Options, Reply) :-

faas:invoke(MRN, Options, Payload, Reply) :-
    atomic_list_concat([mrn, azure, lambda, _Region, Project, Domain, Function],
                       ':', MRN), !,
    ( getenv('AZURE_HOSTKEY', HostKey)
      -> true
      ;  throw(existence_error(getenv, 'AZURE_HOSTKEY'))
    ),
    atomic_list_concat(['https://', Project, '.', Domain, '/api/', Function,
                        '?code=', HostKey], URL),
    proxy_utils:http_proxy(URL, ProxyOptions),
    AzureOptions = [ status_code(Code) %%, cert_verify_hook(cert_accept_any)
                     | ProxyOptions],
    merge_options(AzureOptions, Options, MergedOptions),
    json_utils:term_json_dict(Json, Payload),
    http_post(URL, json(Json), R1, MergedOptions),
    ( Code = 200
      -> json_utils:term_json_dict(R1, Reply)
      ;  Reply = _{error: R1, cause: status_code(Code)}
    ).
