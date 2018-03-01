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

:- module(proxy_utils,
          [ http_proxy/2
          ]).


http_proxy(DistHost, Options) :-
    set_proxy(DistHost, Options)
    -> true
    ;  Options = [].

set_proxy(DistUrl, Options) :-
    parse_url(DistUrl, DistComponents),
    subset([protocol(DistProtocol), host(DistHost)], DistComponents),
    ( DistProtocol = http
      -> getenv('HTTP_PROXY', HttpProxy)
      ;  getenv('HTTPS_PROXY', HttpProxy)
    ),
    ( getenv('NO_PROXY', NoProxy)
      -> split_string(NoProxy, ',', ' ', NoProxyList)
      ;  NoProxyList = []
    ),
    parse_url(HttpProxy, ProxyComponents),
    subset([host(ProxyHost), port(ProxyPort)], ProxyComponents),
    ( atom_string(DistHost, DistHostStr),
      memberchk(DistHostStr, NoProxyList)
      -> Options = []
      ;  ( memberchk(user(User), ProxyComponents)
           -> ( atomic_list_concat([ID, PW], ':', User)
                -> Options = [proxy_authorization(basic(ID, PW)),
                              proxy(ProxyHost:ProxyPort)]
                   ;  throw(error(http_proxy(HttpProxy), getenv))
                 )
           ;  Options = [proxy(ProxyHost:ProxyPort)]
         )
    ).
