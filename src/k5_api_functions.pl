%% -*- mode: prolog; coding: utf-8; -*-
%%
%% Copyright 2018 FUJITSU LIMITED
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

:- module(k5_api_functions,
          [ faas:invoke/4
         ]).

:- use_module(k5_api_utils).
:- use_module(json_utils).
:- use_module(proxy_utils).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/http_sgml_plugin)).
:- use_module(library(http/json)).


:- multifile
       faas:invoke/4.

faas:invoke(URI, Options, Payload, Reply) :-
    atomic_list_concat([frn, k5, functions, _, _, function, Function],
                       ':', URI), !,
    k5_api_utils:k5(K5Options),
    option(k5_apihost(K5ApiHost), K5Options),
    atomic_list_concat(['https://',
                        K5ApiHost,
                        '/2015-09-01'
                       ], URL),
    merge_options(Options, K5Options, MergedOptions),
    atom_json_dict(Body, Payload, []),
    atomic_list_concat(['ScriptIdentifier=', Function,
                        '&Method=POST',
                        '&Body=', Body], Form),

    http_post(URL, atom('application/x-www-form-urlencoded', Form), R1,
              [ request_header('X-Amz-Target'='2015-09-01.ExecuteScript'),
                cert_verify_hook(cert_accept_any)
              | MergedOptions
              ]),
    xpath(R1, //('ResponseData'(content)), [R2|_]),
    atom_json_dict(R2, Reply, []).
