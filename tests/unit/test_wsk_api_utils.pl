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

:- include('../../src/wsk_api_utils.pl').

%%
%% Unit Tests
%%
:- use_module(library(plunit)).

:- begin_tests(utils).

test(ns_echo, (NS, ActionName) = ("whisk.system", "utils/echo")) :-
    api_action_name("/whisk.system/utils/echo", NS, ActionName).

test(ns_hello, (NS, ActionName) = ("guest", "hello")) :-
    api_action_name('/guest/hello', NS, ActionName).

test(ns_none, (NS, ActionName) = ("whisk.system", none)) :-
    api_action_name("/whisk.system", NS, ActionName).

test(echo, (NS, ActionName) = (default, "utils/echo")) :-
    api_action_name("utils/echo", NS, ActionName).

test(json_term_to_dict, Name  == "openwhisk") :-
    term_json_dict(json([name=openwhisk]), Dict), Name = Dict.name.

test(json_dict_to_term, Term  == json([name=openwhisk])) :-
    term_json_dict(Term, json{name:"openwhisk"}).

:- end_tests(utils).

:- begin_tests(options).

test(only_ip, (PROTOCOL, HOST, PORT) = (https, '12.34.56.78', 443)) :-
    setenv('APIHOST', "12.34.56.78"),
    openwhisk(Options),
    option(protocol(PROTOCOL), Options),
    option(api_host(HOST), Options),
    option(port(PORT), Options).

test(http_ip, (PROTOCOL, HOST, PORT) = (http, '12.34.56.78', 80)) :-
    setenv('APIHOST', "http://12.34.56.78"),
    openwhisk(Options),
    option(protocol(PROTOCOL), Options),
    option(api_host(HOST), Options),
    option(port(PORT), Options).

test(https_ip, (PROTOCOL, HOST, PORT) = (https, '12.34.56.78', 443)) :-
    setenv('APIHOST', "https://12.34.56.78"),
    openwhisk(Options),
    option(protocol(PROTOCOL), Options),
    option(api_host(HOST), Options),
    option(port(PORT), Options).

test(nginx, (PROTOCOL, HOST, PORT) = (https, '168.1.145.47', 30218)) :-
    unsetenv('APIHOST'),
    setenv('NGINX_SERVICE_HOST', "168.1.145.47"),
    setenv('NGINX_SERVICE_PORT_HTTPS_API', 30281),
    openwhisk(_Options),
    option(protocol(PROTOCOL), Options),
    option(api_host(HOST), Options),
    option(port(PORT), Options).

test(unknown, error(unknown_api_host, _)) :-
    unsetenv('APIHOST'),
    unsetenv('NGINX_SERVICE_HOST'),
    openwhisk(_Options).

:- end_tests(options).

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
