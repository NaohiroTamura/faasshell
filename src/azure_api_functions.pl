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

:- module(azure_api_functions,
          [ faas:list/3,
            faas:invoke/4
         ]).

:- use_module(azure_api_utils).
:- use_module(json_utils).
:- use_module(proxy_utils).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).


:- multifile
       faas:list/3,
       faas:invoke/4.

%%
subscription_id(Options, Reply) :-
    azure(AzureOptions),
    merge_options(Options, AzureOptions, MergedOptions),
    URL = 'https://management.azure.com/subscriptions?api-version=2016-06-01',
    http_get(URL, R1, MergedOptions),
    json_utils:term_json_dict(R1, R2),
    maplist([In, Out]>>(_{state: "Enabled", subscriptionId: Out} :< In),
            R2.value, Reply).

resource_groups([], _Options, []).
resource_groups([SubscriptionId | SubscriptionIds], Options, Reply) :-
    azure(AzureOptions),
    merge_options(Options, AzureOptions, MergedOptions),
    atomic_list_concat(['https://management.azure.com/subscriptions/',
                        SubscriptionId,
                        '/resourcegroups?api-version=2017-05-10'],
                        URL),
    http_get(URL, R1, MergedOptions),
    json_utils:term_json_dict(R1, R2),
    length(R2.value, N),
    length(S, N),
    maplist(=(SubscriptionId), S),
    maplist([SId, In, (SId, Out)]>>(_{name: Out} :< In), S, R2.value, R3),
    resource_groups(SubscriptionIds, Options, R4),
    append(R3, R4, Reply).

sites([], _Options, []).
sites([SubscriptionId | SubscriptionIds], Options, Reply) :-
    azure(AzureOptions),
    merge_options(Options, AzureOptions, MergedOptions),
    atomic_list_concat(['https://management.azure.com/subscriptions/',
                        SubscriptionId,
                        '/providers/Microsoft.Web/sites?api-version=2016-08-01'],
                        URL),
    http_get(URL, R1, MergedOptions),
    json_utils:term_json_dict(R1, R2),
    length(R2.value, N),
    length(S, N),
    maplist(=(SubscriptionId), S),
    maplist([SId, In, (SId, ResourceGroup, Name)]>>(
                _{name: Name, kind: "functionapp",
                  properties: Properties} :< In,
                _{resourceGroup: ResourceGroup} :< Properties),
            S, R2.value, R3),

    sites(SubscriptionIds, Options, R4),
    append(R3, R4, Reply).

functions([], _Options, []).
functions([(SubscriptionId, ResouceGroup, Site) | T], Options, Reply) :-
    azure(AzureOptions),
    atomic_list_concat(['https://management.azure.com/subscriptions/',
                        SubscriptionId,
                        '/resourceGroups/',
                        ResouceGroup,
                        '/providers/Microsoft.Web/sites/',
                        Site,
                        '/functions',
                        '?api-version=2016-08-01'],
                        URL),
    merge_options(Options, AzureOptions, MergedOptions),
    http_get(URL, R1, MergedOptions),
    json_utils:term_json_dict(R1, R2),
    get_dict('value', R2, R3),
    functions(T, Options, R4),
    append(R3, R4, Reply).

%%
faas:list([], Options, Reply) :-
    subscription_id(Options, SubscriptionIdList),
    sites(SubscriptionIdList, Options, Tuple),
    functions(Tuple, Options, Reply).

faas:list(FRN, Options, Reply) :-
    atom(FRN), !,
    atomic_list_concat([frn, azure, functions, _Location, Site, function, Function],
                       ':', FRN),
    subscription_id(Options, SubscriptionIdList),
    sites(SubscriptionIdList, Options, Tuple),
    atom_string(Site, SiteStr),
    memberchk((SubscriptionId, ResouceGroup, SiteStr), Tuple),
    azure(AzureOptions),
    atomic_list_concat(['https://management.azure.com/subscriptions/',
                        SubscriptionId,
                        '/resourceGroups/',
                        ResouceGroup,
                        '/providers/Microsoft.Web/sites/',
                        Site,
                        '/functions/',
                        Function,
                        '?api-version=2016-08-01'],
                        URL),
    merge_options(Options, AzureOptions, MergedOptions),
    http_get(URL, R1, MergedOptions),
    json_utils:term_json_dict(R1, Reply).

%%
faas:invoke(FRN, Options, Payload, Reply) :-
    atomic_list_concat([frn, azure, functions, _Location, Site, function, Function],
                       ':', FRN), !,
    ( getenv('AZURE_HOSTKEY', HostKey)
      -> true
      ;  throw(existence_error(getenv, 'AZURE_HOSTKEY'))
    ),
    atomic_list_concat(['https://', Site, '.azurewebsites.net/api/', Function,
                        '?code=', HostKey], URL),
    proxy_utils:http_proxy(URL, ProxyOptions),
    AzureOptions = [ status_code(_) %%, cert_verify_hook(cert_accept_any)
                     | ProxyOptions],
    merge_options(Options, AzureOptions, MergedOptions),
    json_utils:term_json_dict(Json, Payload),
    http_post(URL, json(Json), R1, MergedOptions),
    option(status_code(Code), MergedOptions),
    ( Code = 200
      -> json_utils:term_json_dict(R1, Reply)
      ;  Reply = _{error: R1, cause: status_code(Code)}
    ).
