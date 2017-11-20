%% -*- mode: prolog; coding: utf-8; -*-
%%
%% $Id$
%%

:- module(wsk_api_actions,
          [list/3,
           invoke/4
         ]).

:- use_module(wsk_api_dcg).
:- use_module(wsk_api_utils).

:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).

:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).

list(Action, Options, Reply) :-
    wsk_api_utils:api_action_name(Action, NS, ActionName),
    option(api_host(HostName), Options),
    option(namespace(NS), Options, default),
    option(query(Query), Options, []),
    wsk_api_utils:api_url(HostName, 
                          wsk_api_dcg:path(get, NS, actions, ActionName, Query),
                          URL, Options),
    option(api_key(ID, PW), Options),
    option(timeout(Timeout), Options, infinite),
    http_get(URL, R1,
             [%% status_code(_Code),
              timeout(Timeout),
              authorization(basic(ID, PW)),
              cert_verify_hook(cert_accept_any)]),
    wsk_api_utils:term_json_dict(R1, Reply).

invoke(Action, Options, Payload, Reply) :-
    wsk_api_utils:api_action_name(Action, NS, ActionName),
    option(api_host(HostName), Options),
    option(query(Query), Options, [blocking=true,result=true]),
    wsk_api_utils:api_url(HostName, 
                          wsk_api_dcg:path(post, NS, actions, ActionName, Query),
                          URL, Options),
    option(api_key(ID, PW), Options),
    option(timeout(Timeout), Options, infinite),
    wsk_api_utils:term_json_dict(Json, Payload),
    http_post(URL, json(Json), R1,
              [%% status_code(_Code),
               timeout(Timeout),
               authorization(basic(ID, PW)),
               cert_verify_hook(cert_accept_any)]),
    wsk_api_utils:term_json_dict(R1, Reply).


%%
%% Unit Tests
%%
:- use_module(library(plunit)).

:- begin_tests(list).

test(json_term_to_dict, Name  == "openwhisk") :-
    term_json_dict(json([name=openwhisk]), Dict), Name = Dict.name.

test(json_dict_to_term, Term  == json([name=openwhisk])) :-
    term_json_dict(Term, json{name:"openwhisk"}).

test(action_list, true) :-
   wsk_api_utils:openwhisk(Options), wsk_api_actions:list(hello, Options, _R). 

:- end_tests(list).

:- begin_tests(invoke).
%% 
test(hello, R = _{payload:"Hello, wsk!"}) :-
    wsk_api_utils:openwhisk(Options), 
    wsk_api_actions:invoke(hello,Options,_{name:"wsk"}, R).

test(echo, R = _{foo:1}) :-
    wsk_api_utils:openwhisk(Options),
    wsk_api_actions:invoke("/whisk.system/utils/echo",Options,_{foo:1}, R).

:- end_tests(invoke).
