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

:- module(k5_api_utils,
          [ k5/1
         ]).

:- use_module(json_utils).
:- use_module(proxy_utils).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).

%%
%% $ swipl -q -l src/k5_api_utils.pl \
%%         -g k5_api_utils:debug_k5 -g 'k5_api_utils:k5(Options)'
debug_k5 :- debug(k5 > user_error),
             debug(k5_cache > user_error).

%%
:- dynamic
       cached_k5_token/2. % cached_azure(Token, Time)

k5(Options) :-
    ( cached_k5_token(CachedOptions, Time),
      get_time(Now),
      option(k5_token_expires_in(ExpiresIn), CachedOptions),
      Now-Time =< abs(ExpiresIn-60) %% renew before 1 min to expire
    )
    -> debug(k5_cache, '~w~n', [cache_hit(CachedOptions, Time)]),
       Options = CachedOptions
    ;  ( retract(cached_k5_token(CachedOptions, Time))
         -> debug(k5_cache, '~w~n', [cache_retracted(CachedOptions,Time)])
         ;  debug(k5_cache, '~w~n', [cache_not_exist(CachedOptions,Time)])
       ),
       debug(supress, '~w~n', [supress_compiler_warning(CachedOptions,Time)]),
       get_time(Renew),
       k5_renew(Options),
       assertz(cached_k5_token(Options, Renew)),
       debug(k5_cache, '~w~n', [cache_asserted(Options, Renew)]).

k5_renew(Options) :-
    proxy_utils:http_proxy('https://www.fujitsu.com', ProxyOptions),
    debug(k5, '~w~n', [proxy_options(ProxyOptions)]),
    getenv('K5_AUTH', K5Auth), K5Auth \== '',
    getenv('K5_APIHOST', K5ApiHost), K5ApiHost \== '',
    split_string(K5Auth, ':', "", [ID, PW]),

    access_token_request(ID, PW, Reply),
    debug(k5, '~w~n', [token_type(Reply)]),
    atom_string(AccessToken, Reply.access_token),
    debug(k5, '~w~n', [access_token(AccessToken)]),
    Options = [ request_header('X-Access-Token'=AccessToken),
                k5_apihost(K5ApiHost),
                k5_token_expires_in(Reply.expires_in)
              | ProxyOptions ].

access_token_request(ID, PW, Reply) :-
    uri_encoded(query_value, ID, IDEnc),
    uri_encoded(query_value, PW, PWEnc),
    atomic_list_concat(['grant_type=client_credentials&scope=service_contract',
                        '&client_id=', IDEnc,
                        '&client_secret=', PWEnc], Payload),
    URL='https://appf-DCM0000000021-APPELB-d308f2bea9324969baa63aba26350e54.loadbalancing-jp-west-1.cloud.global.fujitsu.com/API/oauth2/token',

    proxy_utils:http_proxy(URL, ProxyOptions),
    http_post(URL, atom('application/x-www-form-urlencoded',Payload), R,
              [ request_header('Accept'='application/json'),
                cert_verify_hook(cert_accept_any),
                status_code(_Code)
              | ProxyOptions
              ]),
    debug(k5, '~w~n', [reply_term(R)]),
    json_utils:term_json_dict(R, R2),
    ( is_dict(R2)
      -> Reply = R2
      ;  atom_json_dict(R, Reply, [])
    ),
    debug(k5, '~w~n', [reply_dict(Reply)]).
