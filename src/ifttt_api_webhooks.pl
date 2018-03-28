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

:- module(ifttt_api_webhooks,
          [ faas:invoke/4
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
       faas:invoke/4.

faas:invoke(URI, Options, Payload, Reply) :-
    atomic_list_concat([frn, ifttt, webhooks, _, _, function, Applet],
                       ':', URI), !,
    ( getenv('IFTTT_KEY', IFTTT_KEY)
    ; IFTTT_KEY = ''
    ), !,
    atomic_list_concat(['https://maker.ifttt.com/trigger/',
                        Applet,
                        '/with/key/',
                        IFTTT_KEY
                       ], URL),
    proxy_utils:http_proxy(URL, ProxyOptions),
    merge_options(Options, ProxyOptions, MergedOptions),
    json_utils:term_json_dict(Json, Payload),
    http_post(URL, json(Json), R1, MergedOptions),
    json_utils:term_json_dict(R1, Reply).
