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

%%
%% Kafka API
%%
:- module(kafka_api, []).

:- use_module(library(jpl)).

:- multifile
       faas:mq_init/1,
       faas:activity_start/4,
       faas:activity_started/4,
       faas:activity_end/5,
       faas:activity_ended/5,
       faas:activity_heartbeat/3,
       faas:activity_heartbeated/4,
       faas:event_publish/5,
       faas:event_published/5,
       faas:event_subscribe/4,
       faas:event_subscribed/4.

%%
%% $ swipl -q -l src/kafka_api.pl -g kafka_api:debug_kafka
debug_kafka :- debug(kafka > user_error).

%%
faas:mq_init(kafka) :-
    catch( ( activity_task_queue(kafka(_ , _)),
             debug(kafka, 'faas:mq_init(kafka) has already asserted~n', [])
           ),
           _Error,
           ( message_queue_create(RecvQ),
             gethostname(Hostname),
             uuid(Id),
             atomic_list_concat([Hostname, ':', Id], Group),
             kafka_consumer(Consumer, Group),
             kafka_subscribe(Consumer, faasshell, Group, RecvQ, _ChildId),
             kafka_producer(Producer),
             assertz(activity_task_queue(kafka(Producer, Group, RecvQ))),
             debug(kafka, 'faas:mq_init(kafka) asserted.~n', []),
             sleep(3)  %% needs to wait for establishing Consumer connection
           )
         ).

%%
faas:activity_start(kafka, Activity, TaskToken, InputText) :-
    activity_task_queue(kafka(Producer, Key, MQueue)),

    term_to_atom(get_activity_task(Activity, TaskToken), Value),
    kafka_send_message(Producer, Key, Value),
    debug(kafka, 'send(get_activity_task(~w, ~w))~n', [Activity, TaskToken]),

    thread_get_message(MQueue,
                       reply_activity_task(Activity, TaskToken, InputText),
                       [timeout(60)]),
    debug(kafka, 'recv(reply_activity_task(~w, ~w, ~w))~n',
          [Activity, TaskToken, InputText]).

faas:activity_started(kafka, Activity, InputText, TaskToken) :-
    activity_task_queue(kafka(Producer, Key, MQueue)),

    thread_get_message(MQueue, get_activity_task(Activity, TaskToken)),
    debug(kafka, 'recv(get_activity_task(~w, ~w))~n', [Activity, TaskToken]),

    term_to_atom(reply_activity_task(Activity, TaskToken, InputText), Value),
    kafka_send_message(Producer, Key, Value),
    debug(kafka, 'send(reply_activity_task(~w, ~w, ~w))~n',
          [Activity, TaskToken, InputText]).

faas:activity_end(kafka, Activity, TaskToken, Result, OutputText) :-
    activity_task_queue(kafka(Producer, Key, _MQueue)),

    term_to_atom(send_task_result(Activity, TaskToken, Result, OutputText), Value),
    kafka_send_message(Producer, Key, Value),
    debug(kafka, 'send(send_task_result(~w, ~w, ~w, ~w))~n',
          [Activity, TaskToken, Result, OutputText]).

faas:activity_ended(kafka, Activity, TaskToken, Result, OutputText) :-
    activity_task_queue(kafka(_Producer, _Key, MQueue)),

    thread_get_message(MQueue,
                       send_task_result(Activity, TaskToken, Result,
                                        OutputText)),
    debug(kafka, 'recv(send_task_result(~w, ~w, ~w, ~w))~n',
          [Activity, TaskToken, Result, OutputText]).

faas:activity_heartbeat(kafka, Activity, TaskToken) :-
    activity_task_queue(kafka(Producer, Key, MQueue)),

    term_to_atom(send_task_heartbeat(Activity, TaskToken), Value),
    kafka_send_message(Producer, Key, Value),
    debug(kafka, 'send(send_task_heartbeat(~w, ~w))~n', [Activity, TaskToken]),

    thread_get_message(MQueue,
                       reply_task_heartbeat(Activity, TaskToken),
                       [timeout(60)]),
    debug(kafka, 'recv(reply_task_heartbeat(~w, ~w))~n', [Activity, TaskToken]).

faas:activity_heartbeated(kafka, Activity, TaskToken, HeartbeatSeconds) :-
    activity_task_queue(kafka(Producer, Key, MQueue)),

    thread_get_message(MQueue, send_task_heartbeat(Activity, TaskToken),
                         [timeout(HeartbeatSeconds)])
    -> debug(kafka, 'recv(send_task_heartbeat(~w, ~w))~n', [Activity, TaskToken]),
       term_to_atom(reply_task_heartbeat(Activity, TaskToken), Value),
       kafka_send_message(Producer, Key, Value),
       debug(kafka, 'send(reply_task_heartbeat(~w, ~w))~n', [Activity, TaskToken]).

faas:event_publish(kafka, User, Event, Action, _Timeout) :-
    activity_task_queue(kafka(Producer, Key, _MQueue)),
    debug(kafka, 'faas:event_publish1(kafka, ~w, ~w, ~w)~n', [User, Event, Action]),

    term_to_atom(event_publish(User, Event, Action), Value),
    kafka_send_message(Producer, Key, Value),

    debug(kafka, 'faas:event_publish2(kafka, ~w, ~w, ~w)~n', [User, Event, Action]).

faas:event_published(kafka, User, Event, Action, Timeout) :-
    activity_task_queue(kafka(_Producer, _Key, MQueue)),
    debug(kafka, 'faas:event_published1(kafka, ~w, ~w, ~w)~n',
          [User, Event, Action]),
    thread_get_message(MQueue,
                       event_publish(User, Event, Action), [timeout(Timeout)]),
    debug(kafka, 'faas:event_published2(kafka, ~w, ~w, ~w)~n',
          [User, Event, Action]).

faas:event_subscribe(kafka, User, Event, _Timeout) :-
    activity_task_queue(kafka(Producer, Key, _MQueue)),
    debug(kafka, 'faas:event_subscribe1(kafka, ~w, ~w)~n', [User, Event]),

    term_to_atom(event_subscribe(User, Event), Value),
    kafka_send_message(Producer, Key, Value),

    debug(kafka, 'faas:event_subscribe2(kafka, ~w, ~w)~n', [User, Event]).

faas:event_subscribed(kafka, User, Event, Timeout) :-
    activity_task_queue(kafka(_Producer, _Key, MQueue)),

    debug(kafka, 'faas:event_subscribed1(kafka, ~w, ~w)~n', [User, Event]),
    thread_get_message(MQueue,
                        event_subscribe(User, Event), [timeout(Timeout)]),
    debug(kafka, 'faas:event_subscribed2(kafka, ~w, ~w)~n', [User, Event]).

kafka_send_message(Producer, Key, Value) :-
    kafka_send(Producer, faasshell, Key, Value).

%%
%% $ swipl -q -l src/kafka_api.pl -g kafka_api:debug_jpl
debug_jpl :- debug(jpl > user_error).

%%
kafka_apihost(KafkaApiHost) :-
    getenv('FAASSHELL_KAFKA_APIHOST', KafkaApiHost)
    -> true
    ; ( getenv('KAFKA_SERVICE_HOST', Host),
        getenv('KAFKA_SERVICE_PORT', Port)
        -> atomic_list_concat([Host, ':', Port], KafkaApiHost)
        ;  KafkaApiHost = '127.0.0.1:9092'
      ).

%%
kafka_producer(Producer) :-
    kafka_apihost(KafkaApiHost),
    jpl_new('java.util.Properties', [], KafkaProp),
    jpl_call(KafkaProp, put,
             ['bootstrap.servers', KafkaApiHost], _),
    jpl_call(KafkaProp, put,
             ['key.serializer',
               'org.apache.kafka.common.serialization.StringSerializer'], _),
    jpl_call(KafkaProp, put,
             ['value.serializer',
               'org.apache.kafka.common.serialization.StringSerializer'], _),
    jpl_new('org.apache.kafka.clients.producer.KafkaProducer',
            [KafkaProp], Producer).

kafka_send(Producer, Topic, Key, Value) :-
    jpl_new('org.apache.kafka.clients.producer.ProducerRecord',
            [Topic, Key, Value],
            Record),
    jpl_call(Producer, send, [Record], _RecordMeta).

kafka_close(Producer) :-
    jpl_call(Producer, close, [], _).

%%
kafka_consumer(Consumer, Group) :-
    kafka_apihost(KafkaApiHost),
    jpl_new('java.util.Properties', [], KafkaProp),
    jpl_call(KafkaProp, put,
             ['bootstrap.servers', KafkaApiHost], _),
    jpl_call(KafkaProp, put,
             ['group.id', Group], _),
    jpl_call(KafkaProp, put,
             ['key.deserializer',
               'org.apache.kafka.common.serialization.StringDeserializer'],
             _),
    jpl_call(KafkaProp, put,
             ['value.deserializer',
               'org.apache.kafka.common.serialization.StringDeserializer'],
             _),
    jpl_new('org.apache.kafka.clients.consumer.KafkaConsumer',
            [KafkaProp], Consumer).


kafka_subscribe(Consumer, Topic, Key, ParentId, ChildId) :-
    thread_create(kafka_subscribe(Consumer, Topic, Key, ParentId), ChildId).

kafka_subscribe(Consumer, Topic, Key, ParentId) :-
    catch( ( jpl_call('java.util.Collections', 'singletonList', [Topic],
                      TopicList),
             jpl_call(Consumer, subscribe, [TopicList], _),
             repeat,
               jpl_call(Consumer, poll, [1000], ConsumerRecords),
               jpl_call(ConsumerRecords, iterator, [], RecordsIter),
               debug(jpl, 'consumer_records(in)~n', []),
               iterator(RecordsIter, Topic, Key, ParentId),
               debug(jpl, 'consumer_records(out)~n', []),
               fail
           ),
           Error,
           ( jpl_call(Consumer, unsubscribe, [], _),
             jpl_call(Consumer, close, [], _),
             format(atom(Message), 'kafka_subscribe(catch(~w))', [Error]),
             kafka_log(Message)
           )
         ).

iterator(RecordsIter, Topic, Key, ParentId) :-
    debug(jpl, 'records_iter(in)', []),
    ( jpl_iterator_element(RecordsIter, Record)
      -> jpl_call(Record, topic, [], T),
         jpl_call(Record, key, [], K),
         jpl_call(Record, value, [], V),
         format(atom(Message), '~w : ~w = ~w~n', [T, K, V]),
         kafka_log(Message),
         ( Key = K
           -> term_to_atom(Value, V),
              thread_send_message(ParentId, Value)
           ;  true
         ),
         iterator(RecordsIter, Topic, Key, ParentId)
      ;  debug(jpl, 'records_iter(out)~n', [])
    ).

kafka_unsubscribe(Consumer, ThreadId) :-
    jpl_call(Consumer, wakeup, [], _),
    %% thread_signal(ThreadId, throw(unsibscribe)),
    thread_join(ThreadId, _Status).

%%
kafka_log(Message) :-
    jpl_classname_to_class('org.jpl7.fli.Prolog', JPL),
    jpl_call('org.slf4j.LoggerFactory', 'getLogger', [JPL], Logger),
    jpl_call(Logger, 'info', [Message], _).
