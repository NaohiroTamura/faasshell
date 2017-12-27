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
    db_url(URL, []).

:- end_tests(db_url).

:- begin_tests(db_path).

test(default, Path = ["/", "unit_test"]) :-
    db_path("unit_test", Path, []).

test(doc, Path = ["/", "unit_test", "/", "sample"]) :-
    db_path("unit_test", "sample", Path, []).

test(doc_slash, Path = ["/", "unit_test", "/", "guest%2fsample"]) :-
    db_path("unit_test", "guest/sample", Path, []).

:- end_tests(db_path).

:- begin_tests(db_crud).

test(db_create, (Code, Res) = (201, _{ok:true})) :-
    db_create(unit_test_db, Code, Res).

test(db_exist, (Code, Res) = (200, '')) :-
    db_exist(unit_test_db, Code, Res).

test(db_exist2, (Code, Res) = (412, _{error:"file_exists",reason:_})) :-
    db_create(unit_test_db, Code, Res).

test(doc_create, (Code, Res) = (201, _{id:"sample", ok:true, rev:_})) :-
    doc_create(unit_test_db, "sample", _{contents: "hello world!"}, Code, Res).

test(doc_create_conflicted, (Code, Res) = (409, _{error:"conflict", reason:_})) :-
    doc_create(unit_test_db, "sample", _{contents: "hello world!"}, Code, Res).

test(doc_read, (Code, Res) = (200, _{'_id':"sample", '_rev':_, contents:_})) :-
    doc_read(unit_test_db, "sample", Code, Res).

test(doc_read_not_found, (Code, Res) = (404, _{error:"not_found", reason:_})) :-
    doc_read(unit_test_db, "not_existed", Code, Res).

test(doc_update, (Code1, Code2, Res) = (200, 201, _{id:"sample", ok:true, rev:_})) :-
    doc_read(unit_test_db, "sample", Code1, R1),
    doc_update(unit_test_db, "sample",
               R1.'_rev', _{contents: "updated!"}, Code2, Res).

test(doc_update_bad_rev, (Code, Res) = (400, _{error:"bad_request", reason:_})) :-
    doc_update(unit_test_db, "sample",
               "bad_revision", _{contents: "updated!"}, Code, Res).

test(doc_update_wrong_rev,
     (Code, Res) = (500, _{error:"unknown_error", reason:_, ref:_})) :-
    doc_update(unit_test_db, "sample",
               "2-abcdefghijklmnopqrstrvwxyz123456",
               _{contents: "updated!"}, Code, Res).

test(doc_delete_bad_rev, (Code, Res) = (400, _{error:"bad_request", reason:_})) :-
    doc_delete(unit_test_db, "sample", "bad_revision", Code, Res).

test(doc_delete_wrong_rev,
     (Code, Res) = (500, _{error:"unknown_error", reason:_, ref:_})) :-
    doc_delete(unit_test_db, "sample", "2-abcdefghijklmnopqrstrvwxyz123456",
               Code, Res).

test(doc_delete, (Code1, Code2, Res) = (200, 200, _{id:"sample", ok:true, rev:_})) :-
    doc_read(unit_test_db, "sample", Code1, R1),
    doc_delete(unit_test_db, "sample", R1.'_rev', Code2, Res).

test(db_delete, (Code, Res) = (200, _{ok:true})) :-
    db_delete(unit_test_db, Code, Res).

test(db_not_exist, (Code, Res) = (404, _{error:"not_found",reason:_})) :-
    db_delete(unit_test_db, Code, Res).

:- end_tests(db_crud).

:- begin_tests(db_env).

test(local, (ID, PW, URL) = ("id", "pw",
                             ["http", "://", "127.0.0.1", ":", "5984"])) :-
    unsetenv('DB_APIHOST'),
    setenv('DB_AUTH', "id:pw"),
    db_env(Options),
    option(authorization(basic(ID, PW)), Options),
    option(db_url(URL), Options),
    unsetenv('DB_AUTH').

test(kube, (ID, PW, URL) = ("id", "pw",
                            ["http", "://", "172.21.20.197", ":", "5984"])) :-
    unsetenv('DB_APIHOST'),
    setenv('DB_AUTH', "id:pw"),
    setenv('COUCHDB_SERVICE_HOST', '172.21.20.197'),
    setenv('COUCHDB_SERVICE_PORT_COUCHDB', 5984),
    db_env(Options),
    option(authorization(basic(ID, PW)), Options),
    option(db_url(URL), Options),
    unsetenv('DB_AUTH'),
    unsetenv('COUCHDB_SERVICE_HOST'),
    unsetenv('COUCHDB_SERVICE_PORT_COUCHDB').

test(private, (ID, PW, URL) = ("id", "pw",
                               ["http", "://", "test-host.local", ":", "5984"])) :-
    setenv('DB_AUTH', "id:pw"),
    setenv('DB_APIHOST', 'http://test-host.local'),
    db_env(Options),
    option(authorization(basic(ID, PW)), Options),
    option(db_url(URL), Options),
    unsetenv('DB_AUTH'),
    unsetenv('DB_APIHOST').

test(private, (ID, PW, URL) = ("id", "pw",
                               ["http", "://", "test-host.local", ":", "1234"])) :-
    setenv('DB_AUTH', "id:pw"),
    setenv('DB_APIHOST', 'http://test-host.local:1234'),
    db_env(Options),
    option(authorization(basic(ID, PW)), Options),
    option(db_url(URL), Options),
    unsetenv('DB_AUTH'),
    unsetenv('DB_APIHOST').

test(cloud, (ID, PW, URL) = ("id", "pw",
                             ["https", "://", "test-host.com", ":", "443"])) :-
    setenv('DB_AUTH', "id:pw"),
    setenv('DB_APIHOST', 'https://test-host.com'),
    db_env(Options),
    option(authorization(basic(ID, PW)), Options),
    option(db_url(URL), Options),
    unsetenv('DB_AUTH'),
    unsetenv('DB_APIHOST').

test(cloud, (ID, PW, URL) = ("id", "pw",
                             ["https", "://", "test-host.com", ":", "1234"])) :-
    setenv('DB_AUTH', "id:pw"),
    setenv('DB_APIHOST', 'https://test-host.com:1234'),
    db_env(Options),
    option(authorization(basic(ID, PW)), Options),
    option(db_url(URL), Options),
    unsetenv('DB_AUTH'),
    unsetenv('DB_APIHOST').

:- end_tests(db_env).
