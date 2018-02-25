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

:- include('../../src/kafka_api.pl').

%%
%% Unit Tests
%%
:- use_module(library(plunit)).

:- begin_tests(env_var,
               [ setup( ( unsetenv('FAASSHELL_KAFKA_APIHOST'),
                          unsetenv('KAFKA_SERVICE_HOST'),
                          unsetenv('KAFKA_SERVICE_PORT') )
                     ),
                 cleanup( ( unsetenv('FAASSHELL_KAFKA_APIHOST'),
                            unsetenv('KAFKA_SERVICE_HOST'),
                            unsetenv('KAFKA_SERVICE_PORT') )
                     )
              ]).

test(default, KafkaApiHost = '127.0.0.1:9092') :-
    kafka_apihost(KafkaApiHost).

test(k8s, KafkaApiHost = '1.2.3.4:5678') :-
    setenv('KAFKA_SERVICE_HOST', '1.2.3.4'),
    setenv('KAFKA_SERVICE_PORT', '5678'),
    kafka_apihost(KafkaApiHost).

test(sepcific, KafkaApiHost = 'kafka:9093') :-
    setenv('FAASSHELL_KAFKA_APIHOST', 'kafka:9093'),
    kafka_apihost(KafkaApiHost).

:- end_tests(env_var).
