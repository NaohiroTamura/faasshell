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

:- include('unit_test_utils.pl').

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

:- end_tests(utils).

:- begin_tests(options).

test(no_apihost, fail) :-
    unsetenv('WSK_APIHOST'),
    openwhisk(_Options).

test(apihost_zero_length, fail) :-
    setenv('WSK_APIHOST', ''),
    openwhisk(_Options).

test(auth_zero_length,
     (PROTOCOL, HOST, PORT) = (https, 'openwhisk.apache.org', 443)) :-
    setenv('WSK_APIHOST', 'https://openwhisk.apache.org'),
    setenv('WSK_AUTH', ''),
    openwhisk(Options),
    assertion(\+ option(authorization(basic(_ID, _PW)), Options)),
    option(protocol(PROTOCOL), Options),
    option(api_host(HOST), Options),
    option(port(PORT), Options).

test(only_ip, (PROTOCOL, HOST, PORT) = (https, '12.34.56.78', 443)) :-
    setenv('WSK_APIHOST', "12.34.56.78"),
    openwhisk(Options),
    option(protocol(PROTOCOL), Options),
    option(api_host(HOST), Options),
    option(port(PORT), Options).

test(http_ip, (PROTOCOL, HOST, PORT) = (http, '12.34.56.78', 80)) :-
    setenv('WSK_APIHOST', "http://12.34.56.78"),
    openwhisk(Options),
    option(protocol(PROTOCOL), Options),
    option(api_host(HOST), Options),
    option(port(PORT), Options).

test(https_ip, (PROTOCOL, HOST, PORT) = (https, '12.34.56.78', 443)) :-
    setenv('WSK_APIHOST', "https://12.34.56.78"),
    openwhisk(Options),
    option(protocol(PROTOCOL), Options),
    option(api_host(HOST), Options),
    option(port(PORT), Options).

test(https_ip, (PROTOCOL, HOST, PORT) = (https, 'openwhisk.ng.bluemix.net', 443)) :-
    setenv('WSK_APIHOST', "openwhisk.ng.bluemix.net"),
    openwhisk(Options),
    option(protocol(PROTOCOL), Options),
    option(api_host(HOST), Options),
    option(port(PORT), Options).

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
