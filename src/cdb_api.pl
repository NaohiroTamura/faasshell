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

%%
%% CouchDB API
%%

:- module(cdb_api,
          [ db_create/3,
            db_delete/3,
            doc_create/5,
            doc_read/4,
            doc_update/6,
            doc_delete/5
         ]).

:- use_module(json_utils).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).

%% CouchDB API Grammar
db_url() -->
    scheme(default), ["://"], db_host(default), [":"], db_port(default).
db_url(SCHEME, DB_HOST, DB_PORT) -->
    scheme(SCHEME), ["://"], db_host(DB_HOST), [":"], db_port(DB_PORT).

scheme(default)  --> ["http"].
scheme(http)     --> ["http"].
scheme(https)    --> ["https"].

db_host(default) --> ["127.0.0.1"], !.
db_host(DB_HOST) --> [S], { atom_string(DB_HOST, S) }.

db_port(default) --> ["5984"], !.
db_port(DB_PORT) --> [S], { atom_string(DB_PORT, S) }.

db_path(DB)      --> sep, database(DB).

db_path(DB, Doc) --> db_path(DB), sep, document(Doc).

sep              --> ["/"].

database(DB)     --> [S], { atom_string(DB, S) }.

document(Doc)    --> [S], { www_form_encode(Doc,E), atom_string(E, S) }.
%%

%%
db_env(Options) :-
    ( getenv('DB_AUTH', DB_AUTH),
      split_string(DB_AUTH, ':', "", [ID, PW])
      -> AuthOpt = [authorization(basic(ID, PW))]
      ;  AuthOpt = []
    ),
    ( getenv('DB_APIHOST', URL)
      -> parse_url(URL, Attributes),
         option(protocol(Scheme), Attributes),
         option(host(DB_HOST), Attributes),
         ( option(port(PORT), Attributes)
           -> DB_PORT = PORT
           ; ( Scheme = http
               -> DB_PORT = default
               ;  DB_PORT = "443"
             )
         )
      ; ( getenv('COUCHDB_SERVICE_HOST', Host),
          getenv('COUCHDB_SERVICE_PORT_COUCHDB', Port)
          -> Scheme = default,
             atom_string(Host, DB_HOST),
             atom_number(Port, DB_PORT)
          ;  Scheme = default,
             DB_HOST = default,
             DB_PORT = default
        )
    ),
    db_url(Scheme, DB_HOST, DB_PORT, Base, [] ),
    append(AuthOpt, [
               db_url(Base),
               status_code(_Code),
               timeout(10),
               cert_verify_hook(cert_accept_any)
           ], Options).
%%
db_create(DB, Code, Res) :-
    db_env(Options),
    option(db_url(Base), Options),
    option(status_code(Code), Options),
    db_path(DB, Path, []),
    flatten([Base, Path], URLList),
    atomics_to_string(URLList, URL),
    http_put(URL, json(json([])), Data, Options),
    json_utils:term_json_dict(Data, Res).

db_exist(DB, Code, Res) :-
    db_env(Options),
    option(db_url(Base), Options),
    option(status_code(Code), Options),
    db_path(DB, Path, []),
    flatten([Base, Path], URLList),
    atomics_to_string(URLList, URL),
    http_get(URL, Res, [method(head), to(atom) | Options]).

db_delete(DB, Code, Res) :-
    db_env(Options),
    option(db_url(Base), Options),
    option(status_code(Code), Options),
    db_path(DB, Path, []),
    flatten([Base, Path], URLList),
    atomics_to_string(URLList, URL),
    http_delete(URL, Data, Options),
    json_utils:term_json_dict(Data, Res).

%%
doc_create(DB, Doc, Data, Code, Res) :-
    db_env(Options),
    option(db_url(Base), Options),
    option(status_code(Code), Options),
    db_path(DB, Doc, Path, []),
    flatten([Base, Path], URLList),
    atomics_to_string(URLList, URL),
    json_utils:term_json_dict(Json, Data),
    http_put(URL, json(Json), Reply, Options),
    json_utils:term_json_dict(Reply, Res).

doc_read(DB, Doc, Code, Res) :-
    db_env(Options),
    option(db_url(Base), Options),
    option(status_code(Code), Options),
    db_path(DB, Doc, Path, []),
    flatten([Base, Path], URLList),
    atomics_to_string(URLList, URL),
    http_get(URL, Reply, Options),
    json_utils:term_json_dict(Reply, Res).

doc_update(DB, Doc, Rev, Data, Code, Res) :-
    db_env(Options),
    option(db_url(Base), Options),
    option(status_code(Code), Options),
    db_path(DB, Doc, Path, []),
    flatten([Base, Path], URLList),
    atomics_to_string(URLList, URL),
    DataRev = Data.put(_{'_rev': Rev}),
    json_utils:term_json_dict(Json, DataRev),
    http_put(URL, json(Json), Reply, Options),
    json_utils:term_json_dict(Reply, Res).

doc_delete(DB, Doc, Rev, Code, Res) :-
    db_env(Options),
    option(db_url(Base), Options),
    option(status_code(Code), Options),
    db_path(DB, Doc, Path, []),
    flatten([Base, Path, ["?rev=", Rev]], URLList),
    atomics_to_string(URLList, URL),
    http_delete(URL, Reply, Options),
    json_utils:term_json_dict(Reply, Res).
