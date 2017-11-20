%% -*- mode: prolog; coding: utf-8; -*-
%%
%% $Id$
%%

:- module(wsk_api_utils,
          [ openwhisk/1,
            api_url/4,
            api_action_name/3,
            term_json_dict/2
         ]).

:- use_module(wsk_api_dcg).

:- use_module(library(http/json)).

api_key(Key, ID, PW) :-
    split_string(Key, ':', "", [ID, PW]).

openwhisk(Options) :-
    %% prerequisite
    %% $ export $(grep AUTH ~/.wskprops)
    %% $ export $(grep APIHOST ~/.wskprops)
    getenv('AUTH',Key),
    api_key(Key, ID, PW),
    getenv('APIHOST',HOST),
    Options = [
        api_key(ID, PW),
        api_host(HOST),
        protocol(https),
        port(443),
        namespace(default)
    ].

api_scheme(http).
api_scheme(https).

api_host(HostName) :- string(HostName).

api_port(Port) :- number(Port).

api_version("v1").

api_url(ApiHost, Gen, URL, Options) :-
    phrase(Gen, [_|Path]),
    option(protocol(Protocol), Options, https),
    api_scheme(Protocol),
    option(port(Port), Options, 443),
    api_port(Port),
    api_version(Ver),
    append([Protocol, "://", ApiHost, ":", Port, "/api/", Ver], Path, URLList),
    atomics_to_string(URLList, URL).

api_action_name(Action, NS, ActionName) :-
    split_string(Action, "/", "", ["", NS | AN])
    -> atomics_to_string(AN, "/", ActionName)
    ; ActionName = Action,
      NS = default.

term_json_dict(Term, Dict) :-
    ground(Term), !,
    atom_json_term(Atom, Term, []), atom_json_dict(Atom, Dict, []).
term_json_dict(Term, Dict) :-
    atom_json_dict(Atom, Dict, []), atom_json_term(Atom, Term, []).


%%
%% Unit Tests
%%
:- use_module(library(plunit)).

:- begin_tests(utils).

test(ns_echo, (NS, ActionName) = ("whisk.system", "utils/echo")) :-
    api_action_name("/whisk.system/utils/echo", NS, ActionName).

test(echo, (NS, ActionName) = (default, "utils/echo")) :-
    api_action_name("utils/echo", NS, ActionName).

test(json_term_to_dict, Name  == "openwhisk") :-
    term_json_dict(json([name=openwhisk]), Dict), Name = Dict.name.

test(json_dict_to_term, Term  == json([name=openwhisk])) :-
    term_json_dict(Term, json{name:"openwhisk"}).

:- end_tests(utils).

:- begin_tests(api_url).

test(https, URL = "https://bluemix:443/api/v1/namespaces") :-
     api_url("bluemix", wsk_api_dcg:path(get), URL, []).

test(http, URL = "http://bluemix:80/api/v1/namespaces") :-
     api_url("bluemix", wsk_api_dcg:path(get), URL, [protocol(http), port(80)]).

test(ftp, fail) :-
     api_url("bluemix", wsk_api_dcg:path(get), _URL, [protocol(ftp)]).

test(port_string, fail) :-
     api_url("bluemix", wsk_api_dcg:path(get), _URL, [port("80")]).

:- end_tests(api_url).
