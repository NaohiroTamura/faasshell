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

:- module(wsk_api_actions,
          [list/3,
           create/4,
           update/4,
           faas:invoke/4,
           delete/3
         ]).

:- use_module(wsk_api_dcg).
:- use_module(wsk_api_utils).
:- use_module(json_utils).

:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).

:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).

%% wsk_url(+Method, +Action, +Options, +DefaultQuery, -URL, -ID-PW, -Timeout) :-
wsk_url(Method, Action, Options, DefaultQuery, URL, ID-PW, Timeout) :-
    wsk_api_utils:api_action_name(Action, NS, ActionName),
    option(api_host(HostName), Options),
    option(query(Query), Options, DefaultQuery),
    wsk_api_utils:api_url(HostName, 
                          wsk_api_dcg:path(Method, NS, actions, ActionName, Query),
                          URL, Options),
    option(api_key(ID-PW), Options),
    option(timeout(Timeout), Options, infinite).

%%
list(Action, Options, Reply) :-
    wsk_url(get, Action, Options, [], URL, ID-PW, Timeout),
    http_get(URL, R1,
             [%% status_code(_Code),
              timeout(Timeout),
              authorization(basic(ID, PW)),
              cert_verify_hook(cert_accept_any)]),
    json_utils:term_json_dict(R1, Reply).

create(Action, Options, Payload, Reply) :-
    wsk_url(put, Action, Options, [], URL, ID-PW, Timeout),
    json_utils:term_json_dict(Json, Payload),
    http_put(URL, json(Json), R1,
              [%% status_code(_Code),
               timeout(Timeout),
               authorization(basic(ID, PW)),
               cert_verify_hook(cert_accept_any)]),
    json_utils:term_json_dict(R1, Reply).

update(Action, Options, Payload, Reply) :-
    wsk_url(put, Action, Options, [overwrite=true], URL, ID-PW, Timeout),
    json_utils:term_json_dict(Json, Payload),
    http_put(URL, json(Json), R1,
              [%% status_code(_Code),
               timeout(Timeout),
               authorization(basic(ID, PW)),
               cert_verify_hook(cert_accept_any)]),
    json_utils:term_json_dict(R1, Reply).

:- multifile
       faas:invoke/4.

faas:invoke(WRN, Options, Payload, Reply) :-
    atomic_list_concat([wsk, Action], ':', WRN), !,
    %% writeln(wsk(invoke(Action))),
    wsk_url(post, Action, Options, [blocking=true,result=true], URL, ID-PW, Timeout),
    json_utils:term_json_dict(Json, Payload),
    http_post(URL, json(Json), R1,
              [%% status_code(_Code),
               timeout(Timeout),
               authorization(basic(ID, PW)),
               cert_verify_hook(cert_accept_any)]),
    json_utils:term_json_dict(R1, Reply).

delete(Action, Options, Reply) :-
    wsk_url(delete, Action, Options, [], URL, ID-PW, Timeout),
    http_delete(URL, R1,
             [%% status_code(_Code),
              timeout(Timeout),
              authorization(basic(ID, PW)),
              cert_verify_hook(cert_accept_any)]),
    json_utils:term_json_dict(R1, Reply).
