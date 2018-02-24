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
            db_exist/3,
            db_read/3,
            db_delete/3,
            doc_create/5,
            doc_read/4,
            doc_update/5,
            doc_delete/4,
            design_create/5,
            design_read/4,
            design_update/5,
            design_delete/4,
            view_read/6,
            db_init/0,
            db_init/3,
            get_user/2
         ]).

:- use_module(json_utils).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).

%% CouchDB API Grammar
db_url_base() -->
    scheme(default), ["://"], db_host(default), [":"], db_port(default).
db_url_base(SCHEME, DB_HOST, DB_PORT) -->
    scheme(SCHEME), ["://"], db_host(DB_HOST), [":"], db_port(DB_PORT).

scheme(default)  --> ["http"].
scheme(http)     --> ["http"].
scheme(https)    --> ["https"].

db_host(default) --> ["127.0.0.1"], !.
db_host(DB_HOST) --> [S], { atom_string(DB_HOST, S) }.

db_port(default) --> ["5984"], !.
db_port(DB_PORT) --> [S], { atom_string(DB_PORT, S) }.

db_path(DB)      --> sep, database(DB).
db_path(DB, Doc) --> db_path(DB), sep, doc(Doc).
db_path(DB, design, Design) -->
                     db_path(DB), design, doc(Design).
db_path(DB, design, Design, view, View) -->
                     db_path(DB, design, Design), view, doc(View).

database(DB)     --> [S], { atom_string(DB, S) }.

doc(Doc)         --> [S], { www_form_encode(Doc,E), atom_string(E, S) }.

sep              --> ["/"].
design           --> ["/_design/"].
view             --> ["/_view/"].
%%

%%
db_env(Options) :-
    ( getenv('DB_IDENTITY', SubjectDB)
      -> AuthDB = [subject_db(SubjectDB)]
      ;  AuthDB = [subject_db(faasshell_subjects)]
    ),
    ( getenv('DB_AUTH', DB_AUTH),
      split_string(DB_AUTH, ':', "", [ID, PW])
      -> AuthOpt = [authorization(basic(ID, PW)) | AuthDB]
      ;  AuthOpt = AuthDB
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
          getenv('COUCHDB_SERVICE_PORT', Port)
          -> Scheme = default,
             atom_string(Host, DB_HOST),
             atom_number(Port, DB_PORT)
          ;  Scheme = default,
             DB_HOST = default,
             DB_PORT = default
        )
    ),
    db_url_base(Scheme, DB_HOST, DB_PORT, Base, [] ),
    append(AuthOpt, [
               db_url_base(Base),
               status_code(_Code),
               timeout(30),
               cert_verify_hook(cert_accept_any)
           ], Options).

%%
db_url(Path, Query, Options, URL, Code) :-
    db_env(Options),
    option(db_url_base(Base), Options),
    option(status_code(Code), Options),
    flatten([Base, Path, Query], URLList),
    atomics_to_string(URLList, URL).

%%
db_create(DB, Code, Res) :-
    db_path(DB, Path, []),
    db_url(Path, [], Options, URL, Code),
    http_put(URL, json(json([])), Data, Options),
    json_utils:term_json_dict(Data, Res).

db_exist(DB, Code, Res) :-
    db_path(DB, Path, []),
    db_url(Path, [], Options, URL, Code),
    http_get(URL, Res, [method(head), to(atom) | Options]).

db_read(DB, Code, Res) :-
    db_path(DB, Path, []),
    db_url(Path, [], Options, URL, Code),
    http_get(URL, Data, Options),
    json_utils:term_json_dict(Data, Res).

db_delete(DB, Code, Res) :-
    db_path(DB, Path, []),
    db_url(Path, [], Options, URL, Code),
    http_delete(URL, Data, Options),
    json_utils:term_json_dict(Data, Res).

%%
doc_create(DB, Doc, Data, Code, Res) :-
    db_path(DB, Doc, Path, []),
    db_url(Path, [], Options, URL, Code),
    json_utils:term_json_dict(Json, Data),
    http_put(URL, json(Json), Reply, Options),
    json_utils:term_json_dict(Reply, Res).

doc_read(DB, Doc, Code, Res) :-
    db_path(DB, Doc, Path, []),
    db_url(Path, [], Options, URL, Code),
    http_get(URL, Reply, Options),
    json_utils:term_json_dict(Reply, Res).

doc_update(DB, Doc, Data, Code, Res) :-
    doc_read(DB, Doc, Code1, R1),
    ( Code1 = 200
      -> db_path(DB, Doc, Path, []),
         db_url(Path, [], Options, URL, Code),
         DataRev = Data.put('_rev', R1.'_rev'),
         json_utils:term_json_dict(Json, DataRev),
         http_put(URL, json(Json), Reply, Options),
         json_utils:term_json_dict(Reply, Res)
      ;  ( Code1 = 404
           -> doc_create(DB, Doc, Data, Code, Res)
           ;  Code = Code1, Res = R1
         )
    ).

doc_delete(DB, Doc, Code, Res) :-
    doc_read(DB, Doc, Code1, R1),
    ( Code1 = 200
      -> db_path(DB, Doc, Path, []), 
         db_url(Path, ["?rev=", R1.'_rev'], Options, URL, Code),
         http_delete(URL, Reply, Options),
         json_utils:term_json_dict(Reply, Res)
      ;  Code = Code1, Res = R1
    ).

%%
design_create(DB, Doc, Data, Code, Res) :-
    db_path(DB, design, Doc, Path, []),
    db_url(Path, [], Options, URL, Code),
    json_utils:term_json_dict(Json, Data),
    http_put(URL, json(Json), Reply, Options),
    json_utils:term_json_dict(Reply, Res).

design_read(DB, Doc, Code, Res) :-
    db_path(DB, design, Doc, Path, []),
    db_url(Path, [], Options, URL, Code),
    http_get(URL, Reply, Options),
    json_utils:term_json_dict(Reply, Res).

design_update(DB, Doc, Data, Code, Res) :-
    design_read(DB, Doc, Code1, R1),
    ( Code1 = 200
      -> db_path(DB, design, Doc, Path, []),
         db_url(Path, [], Options, URL, Code),
         DataRev = Data.put('_rev', R1.'_rev'),
         json_utils:term_json_dict(Json, DataRev),
         http_put(URL, json(Json), Reply, Options),
         json_utils:term_json_dict(Reply, Res)
      ;  ( Code1 = 404
           -> design_create(DB, Doc, Data, Code, Res)
           ;  Code = Code1, Res = R1
         )
    ).

design_delete(DB, Doc, Code, Res) :-
    design_read(DB, Doc, Code1, R1),
    ( Code1 = 200
      -> db_path(DB, design, Doc, Path, []),
         db_url(Path, ["?rev=", R1.'_rev'], Options, URL, Code),
         http_delete(URL, Reply, Options),
         json_utils:term_json_dict(Reply, Res)
      ;  Code = Code1, Res = R1
    ).

%%
view_read(DB, Design, View, Query, Code, Res) :-
    db_path(DB, design, Design, view, View, Path, []),
    db_url(Path, Query, Options, URL, Code),
    http_get(URL, Reply, Options),
    json_utils:term_json_dict(Reply, Res).

%%
db_init :-
    db_init(faasshell, faasshell, _),
    db_init(faasshell_auths, auths, _),
    ( create_user(demo, 'ec29e90c-188d-11e8-bb72-00163ec1cd01:0b82fe63b6bd450519ade02c3cb8f77ee581f25a810db28f3910e6cdd9d041bf'); true),
    db_init(faasshell_executions, executions, _).

db_init(DB, Doc, [C1, C2, C3, C4]) :-
    db_exist(DB, C1, _R1),
    ( C1 = 200
      -> C2 = null
      ;  db_create(DB, C2, _R2)
    ),
    design_read(DB, Doc, C3, _R3),
    ( C3 = 200
      -> C4 = null
      ;  view_design(Doc, Dict),
         design_create(DB, Doc, Dict, C4, _R4)
    ).

%% CouchDB View
view_design(faasshell,
 _{
   views: _{
     shell: _{
       map: "function (doc) { if (doc.dsl !== undefined) { emit([\"dsl\", doc.namespace], [doc.namespace, doc.name]); } }"
     },
     statemachine: _{
       map: "function (doc) { if (doc.asl !== undefined) { emit([\"asl\", doc.namespace], [doc.namespace, doc.name]); } }"
     }
   },
   language: "javascript"
 }).

view_design(auths,
 _{
   views: _{
     identities: _{
       map: "function (doc) { emit([doc.uid, doc.password], doc.name) }"
     }
   },
   language: "javascript"
 }).

view_design(executions,
 _{
   views: _{
     executions: _{
       map: "function (doc) { emit([doc.namespace, doc.start], [doc.statemachine, doc.result])}"
     }
   },
   language: "javascript"
 }).


%%
create_user(Name, Credential) :-
    atom(Name), atom(Credential), !,
    atomic_list_concat([Uid, Password], ':', Credential),
    Data = _{ name: Name,
              uid: Uid,
              password: Password
            },
    doc_create(faasshell_auths, Name, Data, _Code, _Res).
create_user(Name, Credential) :-
    atom(Name), var(Credential),
    uuid(Uid),
    crypto_data_hash(Uid, Password, [salt(faasshell)]),
    Data = _{ name: Name,
              uid: Uid,
              password: Password
            },
    doc_create(faasshell_auths, Name, Data, Code, _Res),
    Code = 201
    -> atomic_list_concat([Uid, Password], ':', Credential).

%% get_user(+Name, ?Credential)
get_user(Name, Credential) :-
    atom(Name), !,
    doc_read(faasshell_auths, Name, _Code, R),
    atom_string(Name, NameStr),
    _{'_id': NameStr, '_rev': _, password: Password, name: NameStr, uid: Uid} :< R,
    atomic_list_concat([Uid, Password], ':', Credential).
%% get_user(-Name, +Credential)
get_user(Name, Credential) :-
    var(Name), atom(Credential),
    atomic_list_concat([Uid, Password], ':', Credential),
    format(string(Query), '["~w","~w"]', [Uid, Password]),
    uri_encoded(query_value, Query, EncodedQuery),
    view_read(faasshell_auths, auths, identities,
              ['?key=', EncodedQuery], Code, Dict),
    Code = 200, length(Dict.rows, 1)
    -> [Row0] = Dict.rows,
       atom_string(Uid, UidStr),
       atom_string(Password, PasswordStr),
       _{id: NameStr, key: [UidStr, PasswordStr], value: NameStr} :< Row0,
       atom_string(Name, NameStr).


%% DB Configuration
db_get_reduce_limit(Res) :-
    db_read('_membership', Code1, Res1),
    ( Code1 = 200
      -> maplist([Node, NodeKey-Reply]>>(
                     format(string(Path),
                            '/_node/~w/_config/query_server_config/reduce_limit',
                            [Node]),
                     db_url(Path, [], Options, URL, Code2),
                     http_get(URL, Reply, Options),
                     ( Code2 = 200
                       -> atom_string(NodeKey, Node)
                       ;  json_utils:term_json_dict(Reply, Error),
                          throw(Error)
                     )
                 ), Res1.all_nodes, Pair),
         dict_pairs(Res, _, Pair)
      ; throw(Res1)
    ).

db_set_reduce_limit(Flag, Res) :-
    atom_string(Flag1, Flag),
    db_read('_membership', Code1, Res1),
    ( Code1 = 200
      -> length(Res1.all_nodes, Len),
         length(FlagList, Len),
         maplist(=(Flag1), FlagList),
         maplist([Node, Flag2, NodeKey-Reply]>>(
                     format(string(Path),
                            '/_node/~w/_config/query_server_config/reduce_limit',
                            [Node]),
                     db_url(Path, [], Options, URL, Code2),
                     http_put(URL, json(Flag2), Reply, Options),
                     ( Code2 = 200
                       -> atom_string(NodeKey, Node)
                       ;  json_utils:term_json_dict(Reply, Error),
                          throw(Error)
                     )
                 ), Res1.all_nodes, FlagList, Pair),
         dict_pairs(Res, _, Pair)
      ; throw(Res1)
    ).
