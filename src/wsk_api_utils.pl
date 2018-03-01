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

:- module(wsk_api_utils,
          [ openwhisk/1,
            api_url/4,
            api_action_name/3
         ]).

:- use_module(wsk_api_dcg).
:- use_module(proxy_utils).

:- use_module(library(http/json)).

api_key(Key, ID-PW) :-
    split_string(Key, ':', "", [ID, PW]).

openwhisk(Options) :-
    ( getenv('WSK_AUTH',Key), api_key(Key, ID-PW)
      -> AuthOpt =  [authorization(basic(ID, PW))]
      ;  throw(existence_error(getenv, 'WSK_AUTH'))
    ),
    ( getenv('WSK_APIHOST', APIHOST)
      -> ( re_match("^\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}\\.\\d{1,3}$", APIHOST)
           -> PROTOCOL = https,
              HOST = APIHOST,
              PORT = 443
           ;  parse_url(APIHOST, Attributes),
              option(protocol(PROTOCOL), Attributes),
              option(host(HOST), Attributes),
              default_port(PROTOCOL, DEFAULT_PORT),
              option(port(PORT), Attributes, DEFAULT_PORT)
         )
      ; HOST = '172.17.0.1',
        PROTOCOL = https,
        PORT = 443
    ),
    parse_url(DistHost, [protocol(PROTOCOL), host(HOST)]),
    proxy_utils:http_proxy(DistHost, ProxyOptions),
    flatten([AuthOpt,
             ProxyOptions,
             [api_host(HOST), protocol(PROTOCOL), port(PORT),
              cert_verify_hook(cert_accept_any),
              status_code(_Code)]
            ], Options).

default_port(http, 80).
default_port(https, 443).

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

api_action_name(none, default, none) :- !.
api_action_name(Action, NS, ActionName) :-
    split_string(Action, "/", "", ["", NS | AN])
    -> (length(AN, 0), ActionName = none;
        atomics_to_string(AN, "/", ActionName))
    ; ActionName = Action,
      NS = default.
