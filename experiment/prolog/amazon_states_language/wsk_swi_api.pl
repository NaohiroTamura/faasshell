%% -*- mode: prolog; coding: utf-8; -*-
%%
%% $Id$
%%

%%:- module(openwhisk, []).

:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).

:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).

:- use_module(library(plunit)).

api_key('23bc46b1-71f6-4ed5-8c54-816aa4f8c502:123zO3xZCLrMN6v2BKK1dXYFpXlPkccOFqm12CdAsMgRU4VrNZ9lyGVCGuMDGIwP').

id_pw(ID, PW) :-
    api_key(API_KEY),
    split_string(API_KEY, ':', "", [ID,PW]).

%
% api_path([?API, ?Ver, ?NameSpace, ?Command, ?Entity], ?PATH) 
%
api_path([API, Ver, NameSpace, Command, Entity], PATH) :-
    var(PATH), Entity == "", !,
    atomics_to_string([API, Ver, namespaces, NameSpace, Command], '/', PATH).
api_path([API, Ver, NameSpace, Command, Entity], PATH) :-
    nonvar(PATH), Entity == "", !,
    split_string(PATH, "/", "", [API, Ver, "namespaces", NameSpace, Command]).
api_path([API, Ver, NameSpace, Command, Entity], PATH) :-
    var(PATH), !,
    atomics_to_string([API, Ver, namespaces, NameSpace, Command, Entity],
                      '/', PATH).
api_path([API, Ver, NameSpace, Command, Entity], PATH) :-
    nonvar(PATH),
    split_string(PATH, "/", "",
                 [API, Ver, "namespaces", NameSpace, Command, Entity]).
%
:- begin_tests(api_path).

test(collection_list,
     [API, Ver, NameSpace, Command] == ["api", "v1", "_", "actions"]) :-
    api_path([API ,Ver, NameSpace, Command, ""], "api/v1/namespaces/_/actions").

test(collection_path, PATH == "api/v1/namespaces/_/actions") :-
    api_path(["api", "v1", "_", "actions", ""], PATH).

test(entity_list,
     [API, Ver, NameSpace, Command, Entity] ==
     ["api", "v1", "_", "actions", "hello"]) :-
    api_path([API ,Ver, NameSpace, Command, Entity], 
             "api/v1/namespaces/_/actions/hello").

test(entity_path, PATH == "api/v1/namespaces/_/actions/hello") :-
    api_path(["api", "v1", "_", "actions", "hello"], PATH).

:- end_tests(api_path).
%%%

%
% api_query_str(+QueryOpts, -QueryStr)
%
api_query_str(QueryOpts, QueryStr) :-
    maplist(term_to_atom, QueryOpts, S),
    atomics_to_string(S, '&', QueryStr).
%
:- begin_tests(api_query_str).

test(options, QueryStr == "limit=30&skip=0") :-
    api_query_str([limit=30, skip=0], QueryStr).

:- end_tests(api_query_str).
%%%

%
% api_url([+SCHEME, +HOST, +PORT, +PATH, +QueryOpts], -URL)
%
api_url([SCHEME, HOST, PORT, PATH, []], URL) :- !,
    atomics_to_string([SCHEME, "://", HOST, ":", PORT, "/", PATH], URL).
api_url([SCHEME, HOST, PORT, PATH, QueryOpts], URL) :-
    atom_length(PATH, N1), N1>0,
    length(QueryOpts, N2), N2>0,
    api_query_str(QueryOpts, QueryStr),
    atomics_to_string([SCHEME, "://", HOST, ":", PORT, "/", PATH,
                       "?", QueryStr], URL).
%
:- begin_tests(api_url).

test(url, URL == "https://172.17.0.1:443/") :-
    api_url([https, "172.17.0.1", 443, "", []], URL).
test(query, URL ==
            "https://172.17.0.1:443/api/v1/namespaces/_/actions?limit=30&skip=0") :-
    api_url([https, "172.17.0.1", 443, "api/v1/namespaces/_/actions",
             [limit=30, skip=0]], URL).
test(empty_path, fail) :-
    api_url([https, "172.17.0.1", 443, "", [limit=30, skip=0]], _URL).

:- end_tests(api_url).
%%%

%
% Get all actions
%   GET /namespaces/{namespace}/actions
% Get action information
%   GET /namespaces/{namespace}/actions/{actionName}
%
action(get, NameSpace, ActionName, QueryOpts, Data) :-
    api_path([api, v1, NameSpace, actions, ActionName], PATH),
    api_url([https, "172.17.0.1", 443, PATH, QueryOpts], URL),
    id_pw(ID, PW),
    http_get(URL, Data, [authorization(basic(ID,PW)),
                         cert_verify_hook(cert_accept_any)]).
%
:- begin_tests(action_get).

test(actions) :- action(get, '_', "", [], _D).

test(actions) :- action(get, '_', "hello", [], _D).

:- end_tests(action_get).
%%%

%
% Create or update an action
%   PUT /namespaces/{namespace}/actions/{actionName} 
%
action(put, NameSpace, ActionName, QueryOpts, Data, Reply) :-
    api_path([api, v1, NameSpace, actions, ActionName], PATH),
    api_url([https, "172.17.0.1", 443, PATH, QueryOpts], URL),
    id_pw(ID, PW),
    http_put(URL, json(Data), Reply,
             [authorization(basic(ID,PW)), cert_verify_hook(cert_accept_any)]).
%
:- begin_tests(action_put).
test(hello) :-
    Code = "/**
              * Hello world as an OpenWhisk action.
             */
            function main(params) {
                var name = params.name || 'World';
                return {payload:  'Hello, ' + name + '!'};
            }",
    Json = json([namespace="_",
                 name=hello,
                 exec=json([kind="nodejs:default",
                            code=Code])]),
    action(put, "_", "hello", [], Json, _R).

:- end_tests(action_put).
%%%

%
% Invoke an action
%   POST /namespaces/{namespace}/actions/{actionName}
%
action(post, NameSpace, ActionName, QueryOpts, Data, Reply) :-
    api_path([api, v1, NameSpace, actions, ActionName], PATH),
    api_url([https, "172.17.0.1", 443, PATH, QueryOpts], URL),
    id_pw(ID, PW),
    http_post(URL, json(Data), Reply,
             [authorization(basic(ID,PW)), cert_verify_hook(cert_accept_any)]).
%
:- begin_tests(action_post).

test(hello) :- 
    atom_json_term('{"name":"openwhisk"}', Json, []),
    action(post, "_", "hello", [blocking=true,result=true], Json, _R).

:- end_tests(action_post).
%%%

%
% Delete an action
%   DELETE /namespaces/{namespace}/actions/{actionName}
%
action(delete, NameSpace, ActionName, QueryOpts, Data) :-
    api_path([api, v1, NameSpace, actions, ActionName], PATH),
    api_url([https, "172.17.0.1", 443, PATH, QueryOpts], URL),
    id_pw(ID, PW),
    http_delete(URL, Data, [authorization(basic(ID,PW)),
                            cert_verify_hook(cert_accept_any)]).
%
:- begin_tests(action_delete).

test(hello) :- action(delete, "_", "hello", [], _D).

:- end_tests(action_delete).
%%%
