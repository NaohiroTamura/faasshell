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

:- module(common_api_functions,
          [ faas:invoke/4
         ]).

:- use_module(json_utils).
:- use_module(proxy_utils).
:- use_module(cdb_api).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).

:- use_module(library(sgml)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/turtle)).

:- multifile
       faas:invoke/4.

%%
%% This is adhoc implementation for demo
%%
:- rdf_register_prefix(fs, 'https://naohirotamura.github.io/faasshell/ns/faas#').
:- cdb_api:attach_download_file(faasshell_auths, demo, '/logs/hello_world_task.ttl',
                                _Code, _Res),
   rdf_load('/logs/hello_world_task.ttl', []).

faas:invoke(URI, Options, Payload, Reply) :-
    atom_string(URIAtom, URI),
    uri_components(URIAtom,
                   uri_components(Scheme, Authority, Path, Search, _Fragment)),
    ( Scheme = http ; Scheme = https ), !,
    option(faasshell_auth(NS), Options),
    uri_components(User,
                   uri_components(Scheme, Authority, Path, Search, NS)),
    findall(FRNName,
            ( rdf(User, fs:owns, URIAtom),
              rdf(FRN, fs:invokes, URIAtom),
              rdf(FRN, fs:name, FRNName)
            ), FRNList),
    until_succeed(FRNList, Options, Payload, Reply).

until_succeed([], _, _, _) :- fail.
until_succeed([H|T], Options, Payload, Reply) :-
    H = literal(type('http://www.w3.org/2001/XMLSchema#string', Target)),
    faas:invoke(Target, Options, Payload, Reply)
    -> true
    ;  until_succeed(T, Options, Payload, Reply).
