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

:- module(aws_api_lambda,
          [ faas:list/3,
            faas:invoke/4,
            delete/3
         ]).

:- use_module(aws_api_utils).
:- use_module(json_utils).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).


:- multifile
       faas:list/3,
       faas:invoke/4.

faas:list([], Options, Reply) :-
    %% writeln(aws_list),
    getenv('aws_region', Region),
    atomic_list_concat([arn, aws, lambda, Region, '', function, none], ':', ARN),
    aws_list(ARN, Options, R),
    get_dict('Functions', R, Reply).
faas:list(ARN, Options, Reply) :-
    %% writeln(aws_arn(ARN)),
    aws_list(ARN, Options, Reply).

aws_list(ARN, Options, Reply) :-
    atom(ARN), !,
    atomic_list_concat([_, aws, lambda |_ ], ':', ARN),
    aws_api_utils:aws_lambda(list, ARN, '', '', AwsOptions),
    merge_options(Options, AwsOptions, MergedOptions),
    option(url(URL), MergedOptions),
    http_get(URL, R1, MergedOptions),
    json_utils:term_json_dict(R1, Reply).

faas:invoke(ARN, Options, Payload, Reply) :-
    atomic_list_concat([_, aws, lambda |_ ], ':', ARN), !,
    %% writeln(aws(invoke(ARN))),
    atom_json_dict(PayloadText, Payload, []),
    aws_api_utils:aws_lambda(invoke, ARN, '', PayloadText, AwsOptions),
    merge_options(Options, AwsOptions, MergedOptions),
    option(url(URL), MergedOptions),
    http_post(URL, atom('application/json', PayloadText), R1, MergedOptions),
    ( atomic(R1)
      -> Reply = R1
      ;  %% json_utils:term_json_dict(R1, Reply)
         custom_error(R1, Reply)
    ).

%% process AWS Lambda Function Errors
custom_error(In, Out) :-
    json_utils:term_json_dict(In, O1),
    ( _{errorMessage: _ErrorMessage, errorType: ErrorType,
        stackTrace: _StackTrace } :< O1
      -> Out = _{error: ErrorType, cause: O1}
      ;  Out = O1
    ).

delete(ARN, Options, Reply) :-
    aws_api_utils:aws_lambda(delete, ARN, '', '', AwsOptions),
    merge_options(Options, AwsOptions, MergedOptions),
    option(url(URL), MergedOptions),
    http_delete(URL, R1, MergedOptions),
    option(status_code(Code), MergedOptions),
    ( Code = 204
      -> Reply = _{output: ok, status: Code}
      ;  json_utils:term_json_dict(R1, Reply)
    ).
