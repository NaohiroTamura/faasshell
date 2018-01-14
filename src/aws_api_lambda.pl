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

:- module(aws_api_lambda,
          [ list/3,
            invoke/4,
            delete/3
         ]).

:- use_module(aws_api_utils).
:- use_module(json_utils).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).


list(ARN, Options, Reply) :-
    aws_api_utils:aws_lambda(list, ARN, '', '', Options),
    option(url(URL), Options),
    http_get(URL, R1, Options),
    json_utils:term_json_dict(R1, Reply).

invoke(ARN, Options, Payload, Reply) :-
    atom_json_dict(PayloadText, Payload, []),
    aws_api_utils:aws_lambda(invoke, ARN, '', PayloadText, Options),
    option(url(URL), Options),
    http_post(URL, atom('application/json', PayloadText), R1, Options),
    ( atomic(R1)
      -> Reply = R1
      ;  json_utils:term_json_dict(R1, Reply)
    ).

delete(ARN, Options, Reply) :-
    aws_api_utils:aws_lambda(delete, ARN, '', '', Options),
    option(status_code(Code), Options),
    option(url(URL), Options),
    http_delete(URL, R1, Options),
    ( Code = 204
      -> Reply = _{output: ok, status: Code}
      ;  json_utils:term_json_dict(R1, Reply)
    ).