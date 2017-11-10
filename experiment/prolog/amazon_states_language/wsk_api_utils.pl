%% -*- mode: prolog; coding: utf-8; -*-
%%
%% $Id$
%%

:- module(wsk_api_utils,
          [ openwhisk/1,
            api_url/4,
            term_json_dict/2
         ]).

:- use_module(wsk_api_dcg).

:- use_module(library(http/json)).

api_key('23bc46b1-71f6-4ed5-8c54-816aa4f8c502:123zO3xZCLrMN6v2BKK1dXYFpXlPkccOFqm12CdAsMgRU4VrNZ9lyGVCGuMDGIwP').

api_key(Key, ID, PW) :-
    split_string(Key, ':', "", [ID, PW]).

openwhisk(Options) :-
    api_key(Key),
    api_key(Key, ID, PW),
    Options = [
        api_key(ID, PW),
        api_host("172.17.0.1"),
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
