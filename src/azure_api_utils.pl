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

:- module(azure_api_utils,
          [ azure/1
         ]).

:- use_module(json_utils).
:- use_module(proxy_utils).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).

%%
%% $ swipl -q -l src/azure_api_utils.pl \
%%         -g azure_api_utils:debug_azure -g 'azure_api_utils:azure(Options)'
debug_azure :- debug(azure > user_error),
               debug(azure_cache > user_error).
%%
:- dynamic
       cached_azure_token/2. % cached_azure(Token, Time)

azure(Options) :-
    ( cached_azure_token(CachedOptions, Time),
      get_time(Now),
      Now-Time =< 3300 %% 55 mins, JWT expires in 60 mins
    )
    -> debug(azure_cache, '~w~n', [cache_hit(CachedOptions, Time)]),
       Options = CachedOptions
    ;  ( retract(cached_azure_token(CachedOptions, Time))
         -> debug(azure_cache, '~w~n', [cache_retracted(CachedOptions,Time)])
         ;  debug(azure_cache, '~w~n', [cache_not_exist(CachedOptions,Time)])
       ),
       debug(supress, '~w~n', [supress_compiler_warning(CachedOptions,Time)]),
       get_time(Renew),
       azure_renew(Options),
       assertz(cached_azure_token(Options, Renew)),
       debug(azure_cache, '~w~n', [cache_asserted(Options, Renew)]).

azure_renew(Options) :-
    Resource = 'https://management.azure.com/',
    proxy_utils:http_proxy(Resource, ProxyOptions),

    getenv('AZURE_TENANT_ID', TenantId), TenantId \== '',
    getenv('AZURE_CLIENT_ID', ClientId), ClientId \== '',
    getenv('AZURE_CLIENT_SECRET', ClientSecret), ClientSecret \== '',

    access_token_request(TenantId, ClientId, ClientSecret, Resource, Reply),
    atomics_to_string([Reply.token_type, Reply.access_token], ' ',
                      AuthorizationHeader),
    Options = [ request_header('Authorization'=AuthorizationHeader)
                              | ProxyOptions ].

access_token_request(TenantId, ClientId, ClientSecret, Resource, Reply) :-
    proxy_utils:http_proxy(Resource, ProxyOptions),
    uri_encoded(query_value, Resource, ResourceEnc),
    uri_encoded(query_value, ClientId, ClientIdEnc),
    uri_encoded(query_value, ClientSecret, ClientSecretEnc),
    atomic_list_concat(['grant_type=client_credentials&',
                        'resource=', ResourceEnc, '&',
                        'client_id=', ClientIdEnc, '&',
                        'client_secret=', ClientSecretEnc], Payload),
    atomic_list_concat(['https://login.microsoftonline.com/',
                        TenantId,
                        '/oauth2/token'], URL),

    http_post(URL, atom('application/x-www-form-urlencoded',Payload), R,
              [request_header('Accept'='application/json') | ProxyOptions]),
    json_utils:term_json_dict(R, R2),
    debug(azure, '~w~n', [reply_term(R)]),
    ( is_dict(R2)
      -> Reply = R2
      ;  atom_json_dict(R, Reply, [])
    ),
    debug(azure, '~w~n', [reply_dict(Reply)]).
