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

:- include('../../src/proxy_utils.pl').

%%
%% Unit Tests
%%
:- use_module(library(plunit)).

:- begin_tests(http_proxy,
              [setup( (   unsetenv('HTTP_PROXY'),
                          unsetenv('HTTPS_PROXY'),
                          unsetenv('NO_PROXY') ) ),
               cleanup( ( unsetenv('HTTP_PROXY'),
                          unsetenv('HTTPS_PROXY'),
                          unsetenv('NO_PROXY') ) )]).

test(direct) :-
    assertion(http_proxy('https://www.google.com', [])).

test(http_proxy) :-
    setenv('HTTP_PROXY', 'http://proxy.company.com:8080'),
    setenv('HTTPS_PROXY', 'https://proxy.company.com:8433'),
    setenv('NO_PROXY', '127.0,0,1,localhost,172.17.0.1'),
    http_proxy('http://www.google.com', Options),
    assertion(Options = [proxy('proxy.company.com':8080)]).

test(https_proxy) :-
    setenv('HTTP_PROXY', 'http://proxy.company.com:8080'),
    setenv('HTTPS_PROXY', 'https://proxy.company.com:8433'),
    setenv('NO_PROXY', '127.0,0,1,localhost,172.17.0.1'),
    http_proxy('https://www.google.com', Options),
    assertion(Options = [proxy('proxy.company.com':8433)]).

test(http_auth_proxy) :-
    setenv('HTTP_PROXY', 'http://id:pw@proxy.company.com:8080'),
    setenv('HTTPS_PROXY', 'https://id:pw@proxy.company.com:8433'),
    setenv('NO_PROXY', '127.0,0,1,localhost,172.17.0.1'),
    http_proxy('http://www.google.com', Options),
    assertion(Options = [proxy_authorization(basic(id, pw)),
                         proxy('proxy.company.com':8080)]).

test(https_auth_proxy) :-
    setenv('HTTP_PROXY', 'http://id:pw@proxy.company.com:8080'),
    setenv('HTTPS_PROXY', 'https://id:pw@proxy.company.com:8433'),
    setenv('NO_PROXY', '127.0,0,1,localhost,172.17.0.1'),
    http_proxy('https://www.google.com', Options),
    assertion(Options = [proxy_authorization(basic(id, pw)),
                         proxy('proxy.company.com':8433)]).

test(http_bad_auth_proxy, [error(http_proxy('http://id-pw@proxy.company.com:8080'),
                            getenv)]) :-
    setenv('HTTP_PROXY', 'http://id-pw@proxy.company.com:8080'),
    setenv('HTTPS_PROXY', 'https://id-pw@proxy.company.com:8443'),
    setenv('NO_PROXY', '127.0,0,1,localhost,172.17.0.1'),
    http_proxy('http://www.google.com', _Options).

test(https_bad_auth_proxy,
     [error(http_proxy('https://id-pw@proxy.company.com:8443'), getenv)]) :-
    setenv('HTTP_PROXY', 'http://id-pw@proxy.company.com:8080'),
    setenv('HTTPS_PROXY', 'https://id-pw@proxy.company.com:8443'),
    setenv('NO_PROXY', '127.0,0,1,localhost,172.17.0.1'),
    http_proxy('https://www.google.com', _Options).

test(http_noproxy) :-
    setenv('HTTP_PROXY', 'http://id:pw@proxy.company.com:8080'),
    setenv('HTTPS_PROXY', 'https://id:pw@proxy.company.com:8433'),
    setenv('NO_PROXY', '127.0,0,1,localhost,172.17.0.1'),
    http_proxy('http://172.17.0.1:8080', Options),
    assertion(Options = []).

test(https_noproxy) :-
    setenv('HTTP_PROXY', 'http://id:pw@proxy.company.com:8080'),
    setenv('HTTPS_PROXY', 'https://id:pw@proxy.company.com:8433'),
    setenv('NO_PROXY', '127.0,0,1,localhost,172.17.0.1'),
    http_proxy('https://172.17.0.1:8080', Options),
    assertion(Options = []).

test(https_noproxy_space) :-
    setenv('HTTP_PROXY', 'http://id:pw@proxy.company.com:8080'),
    setenv('HTTPS_PROXY', 'https://id:pw@proxy.company.com:8433'),
    setenv('NO_PROXY', '127.0,0,1, localhost, 172.17.0.1'),
    http_proxy('https://172.17.0.1:8080', Options),
    assertion(Options = []).

test(http_empty_proxy) :-
    setenv('HTTP_PROXY', ''),
    setenv('HTTPS_PROXY', ''),
    setenv('NO_PROXY', '127.0,0,1,localhost,172.17.0.1'),
    http_proxy('http://www.google.com', Options),
    assertion(Options = []).

test(https_empty_proxy) :-
    setenv('HTTP_PROXY', ''),
    setenv('HTTPS_PROXY', ''),
    setenv('NO_PROXY', '127.0,0,1,localhost,172.17.0.1'),
    http_proxy('https://www.google.com', Options),
    assertion(Options = []).

:- end_tests(http_proxy).
