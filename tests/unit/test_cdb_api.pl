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

:- include('../../src/cdb_api.pl').

%%
%% Unit Tests
%%
:- use_module(library(plunit)).

:- begin_tests(db_url).

test(default, URL = ["http", "://", "127.0.0.1", ":", "5984"]) :-
    db_url_base(URL, []).

test(specific, URL = ["https", "://", "test-host.com", ":", "1234"]) :-
    db_url_base(https, 'test-host.com', '1234', URL, []).

test(specific, URL = ["http", "://", "172.0.0.1", ":", "5984"]) :-
    parse_url('172.0.0.1:5984',
              [protocol(Scheme), host(Host), port(Port), path(/)]),
    db_url_base(Scheme, Host, Port, URL, []).

:- end_tests(db_url).

:- begin_tests(db_path).

test(default, Path = ["/", "unit_test"]) :-
    db_path(unit_test, Path, []).

test(doc, Path = ["/", "unit_test", "/", "sample"]) :-
    db_path(unit_test, sample, Path, []).

test(doc_slash, Path = ["/", "unit_test", "/", "guest%2fsample"]) :-
    db_path(unit_test, 'guest/sample', Path, []).

test(design, Path = ["/","unit_test","/_design/","faasshell"]) :-
    db_path(unit_test, design, faasshell, Path, []).

test(view, Path = ["/","unit_test","/_design/","faasshell","/_view/","statemachine"]) :-
    db_path(unit_test, design, faasshell, view, statemachine, Path, []).

:- end_tests(db_path).

:- begin_tests(db_crud).

test(db_create, (Code, Res) = (201, _{ok:true})) :-
    db_create(unit_test_db, Code, Res).

test(db_exist, (Code, Res) = (200, '')) :-
    db_exist(unit_test_db, Code, Res).

test(db_exist2, (Code, Res) = (412, _{error:"file_exists",reason:_})) :-
    db_create(unit_test_db, Code, Res).

test(doc_create, (Code, Res) = (201, _{id:"sample", ok:true, rev:_})) :-
    doc_create(unit_test_db, "sample", _{asl: "hello world!"}, Code, Res).

test(doc_create_conflicted, (Code, Res) = (409, _{error:"conflict", reason:_})) :-
    doc_create(unit_test_db, "sample", _{asl: "hello world!"}, Code, Res).

test(doc_read, (Code, Res) = (200, _{'_id':"sample", '_rev':_, asl:_})) :-
    doc_read(unit_test_db, "sample", Code, Res).

test(doc_read_not_found, (Code, Res) = (404, _{error:"not_found", reason:_})) :-
    doc_read(unit_test_db, "not_existed", Code, Res).

test(doc_update, (Code, Res) = (201, _{id:"sample", ok:true, rev:_})) :-
    doc_update(unit_test_db, "sample",  _{asl: "updated!"}, Code, Res).

test(doc_update_create, (Code1, Code2) = (201, 200)) :-
    doc_update(unit_test_db, "new_sample",  _{asl: "created!"}, Code1, _Res1),
    doc_delete(unit_test_db, "new_sample", Code2, _Res2).

test(doc_delete_not_found, (Code, Res) = (404, _{error:"not_found", reason:_})) :-
    doc_delete(unit_test_db, "not_exited", Code, Res).

test(doc_delete, (Code, Res) = (200, _{id:"sample", ok:true, rev:_})) :-
    doc_delete(unit_test_db, "sample", Code, Res).

test(db_delete, (Code, Res) = (200, _{ok:true})) :-
    db_delete(unit_test_db, Code, Res).

test(db_not_exist, (Code, Res) = (404, _{error:"not_found",reason:_})) :-
    db_delete(unit_test_db, Code, Res).

:- end_tests(db_crud).

:- begin_tests(db_init).

test(all, Codes = [404, 201, 404, 201]) :-
     db_delete(test_db_init, _Code, _Res),
     db_init(test_db_init, faasshell, Codes).

test(ready, Codes = [200, null, 200, null]) :-
    db_init(test_db_init, faasshell, Codes).

test(doc, Codes = [200, null, 404, 201]) :-
    design_delete(test_db_init, faasshell, 200, _Res),
    db_init(test_db_init, faasshell, Codes).

:- end_tests(db_init).

:- begin_tests(db_design).

test(update, (Code, Res) = (201, _{ok:true, id:_, rev:_})) :-
    db_delete(test_db_design, _Code, _Res),
    db_init(test_db_design, faasshell, _Codes),
    view_design(faasshell, Dict),
    design_update(test_db_design, faasshell, Dict, Code, Res).

test(view_rereduce_false, (L1, L2) = (3, 2)) :-
    %% asl, guest
    doc_create(test_db_design, 'guest/sample1.json',
               _{asl:"ASL 1", namespace:guest, name:'sample1.json'}, 201, _),
    doc_create(test_db_design, 'guest/sample2.json',
               _{asl:"ASL 2", namespace:guest, name:'sample2.json'}, 201, _),
    doc_create(test_db_design, 'guest/sample3.json',
               _{asl:"ASL 3", namespace:guest, name:'sample3.json'}, 201, _),
    %% asl, admin
    doc_create(test_db_design, 'admin/sample4.json',
               _{asl:"ASL 4", namespace:admin, name:'sample4.json'}, 201, _),
    doc_create(test_db_design, 'admin/sample5.json',
               _{asl:"ASL 5", namespace:admin, name:'sample5.json'}, 201, _),
    %% dsl, guest
    doc_create(test_db_design, 'guest/sample1.dsl',
               _{dsl:"DSL 1", namespace:guest, name:'sample1.dsl'}, 201, _),
    doc_create(test_db_design, 'guest/sample2.dsl',
               _{dsl:"DSL 2", namespace:guest, name:'sample2.dsl'}, 201, _),
    %% dsl, admin
    doc_create(test_db_design, 'admin/sample3.dsl',
               _{dsl:"DSL 3", namespace:admin, name:'sample3.dsl'}, 201, _),
    doc_create(test_db_design, 'admin/sample4.dsl',
               _{dsl:"DSL 4", namespace:admin, name:'sample4.dsl'}, 201, _),
    doc_create(test_db_design, 'admin/sample5.dsl',
               _{dsl:"DSL 5", namespace:admin, name:'sample5.dsl'}, 201, _),

    uri_encoded(query_value, '["asl","guest"]', Q1),
    view_read(test_db_design, faasshell, statemachine, ['?key=', Q1], 200, Res1),
    length(Res1.rows, L1),

    uri_encoded(query_value, '["dsl","guest"]', Q2),
    view_read(test_db_design, faasshell, shell, ['?key=', Q2], 200, Res2),
    length(Res2.rows, L2).

test(view_rereduce_true, Len = 50) :-
    numlist(4,50,L),
    maplist([N,Code]>>(
                format(string(Id),'guest/sample~w.json',[N]),
                format(string(File),'sample~w.json',[N]),
                format(string(Contents),'ASL ~w',[N]),
                doc_create(test_db_design, Id,
                           _{asl:Contents, namespace:guest, name:File}, Code, _)
            ), L, _),

    uri_encoded(query_value, '["asl","guest"]', Q),
    view_read(test_db_design, faasshell, statemachine, ['?key=', Q], 200, Res),
    length(Res.rows, Len).

:- end_tests(db_design).

:- begin_tests(db_env).

test(local, (ID, PW, URL) = ("id", "pw",
                             ["http", "://", "127.0.0.1", ":", "5984"])) :-
    unsetenv('FAASSHELL_DB_APIHOST'),
    setenv('FAASSHELL_DB_AUTH', "id:pw"),
    db_env(Options),
    option(authorization(basic(ID, PW)), Options),
    option(db_url_base(URL), Options),
    unsetenv('FAASSHELL_DB_AUTH').

test(kube, (ID, PW, URL) = ("id", "pw",
                            ["http", "://", "172.21.20.197", ":", "5984"])) :-
    unsetenv('FAASSHELL_DB_APIHOST'),
    setenv('FAASSHELL_DB_AUTH', "id:pw"),
    setenv('COUCHDB_SERVICE_HOST', '172.21.20.197'),
    setenv('COUCHDB_SERVICE_PORT', 5984),
    db_env(Options),
    option(authorization(basic(ID, PW)), Options),
    option(db_url_base(URL), Options),
    unsetenv('FAASSHELL_DB_AUTH'),
    unsetenv('COUCHDB_SERVICE_HOST'),
    unsetenv('COUCHDB_SERVICE_PORT').

test(private, (ID, PW, URL) = ("id", "pw",
                               ["http", "://", "test-host.local", ":", "5984"])) :-
    setenv('FAASSHELL_DB_AUTH', "id:pw"),
    setenv('FAASSHELL_DB_APIHOST', 'http://test-host.local'),
    db_env(Options),
    option(authorization(basic(ID, PW)), Options),
    option(db_url_base(URL), Options),
    unsetenv('FAASSHELL_DB_AUTH'),
    unsetenv('FAASSHELL_DB_APIHOST').

test(private, (ID, PW, URL) = ("id", "pw",
                               ["http", "://", "test-host.local", ":", "1234"])) :-
    setenv('FAASSHELL_DB_AUTH', "id:pw"),
    setenv('FAASSHELL_DB_APIHOST', 'http://test-host.local:1234'),
    db_env(Options),
    option(authorization(basic(ID, PW)), Options),
    option(db_url_base(URL), Options),
    unsetenv('FAASSHELL_DB_AUTH'),
    unsetenv('FAASSHELL_DB_APIHOST').

test(cloud, (ID, PW, URL) = ("id", "pw",
                             ["https", "://", "test-host.com", ":", "443"])) :-
    setenv('FAASSHELL_DB_AUTH', "id:pw"),
    setenv('FAASSHELL_DB_APIHOST', 'https://test-host.com'),
    db_env(Options),
    option(authorization(basic(ID, PW)), Options),
    option(db_url_base(URL), Options),
    unsetenv('FAASSHELL_DB_AUTH'),
    unsetenv('FAASSHELL_DB_APIHOST').

test(cloud, (ID, PW, URL) = ("id", "pw",
                             ["https", "://", "test-host.com", ":", "1234"])) :-
    setenv('FAASSHELL_DB_AUTH', "id:pw"),
    setenv('FAASSHELL_DB_APIHOST', 'https://test-host.com:1234'),
    db_env(Options),
    option(authorization(basic(ID, PW)), Options),
    option(db_url_base(URL), Options),
    unsetenv('FAASSHELL_DB_AUTH'),
    unsetenv('FAASSHELL_DB_APIHOST').

:- end_tests(db_env).
