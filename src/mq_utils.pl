%% #!/usr/bin/swipl -q
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
%%  Message Queue Utils.
%%

:- module(mq_utils,
         [ mq_init/0,
           activity_start/3,
           activity_started/3,
           activity_end/4,
           activity_ended/4,
           activity_heartbeat/2,
           activity_heartbeated/3,
           event_publish/3,
           event_published/4,
           event_subscribe/2,
           event_subscribed/2
         ]).

:- use_module(kafka_api).

%%
%% $ swipl -q -l src/kafka_api.pl -g mq_utils:debug_mq
debug_mq :- debug(mq > user_error).

%% Message Queue Plugin Interface
:- dynamic
       faasshell_mq/1.

%%
mq_init :-
    ( getenv('FAASSHELL_MQ', MQType), MQType \== '' )
    -> faas:mq_init(MQType),
       assertz(faasshell_mq(MQType))
    ;  faas:mq_init(built_in),
       assertz(faasshell_mq(built_in)).

%% svc ---- activity_start ----> run
%%     <--- activity_started ---
activity_start(Activity, TaskToken, InputText) :-
    faasshell_mq(MQType),
    atom_string(ActivityAtom, Activity),
    atom_string(TaskTokenAtom, TaskToken),
    faas:activity_start(MQType, ActivityAtom, TaskTokenAtom, InputText).

activity_started(Activity, InputText, TaskToken) :-
    faasshell_mq(MQType),
    atom_string(ActivityAtom, Activity),
    faas:activity_started(MQType, ActivityAtom, InputText, TaskToken).

%% svc ---- activity_end ----> run
%%     <--- activity_ended ---
activity_end(Activity, TaskToken, Result, OutputText) :-
    faasshell_mq(MQType),
    atom_string(ActivityAtom, Activity),
    atom_string(TaskTokenAtom, TaskToken),
    faas:activity_end(MQType, ActivityAtom, TaskTokenAtom, Result,
                      OutputText).

activity_ended(Activity, TaskToken, Result, OutputText) :-
    faasshell_mq(MQType),
    atom_string(ActivityAtom, Activity),
    atom_string(TaskTokenAtom, TaskToken),
    faas:activity_ended(MQType, ActivityAtom, TaskTokenAtom, Result,
                   OutputText).

%% svc ---- activity_heartbeat ----> run
%%     <--- activity_heartbeated ---
activity_heartbeat(Activity, TaskToken) :-
    faasshell_mq(MQType),
    atom_string(ActivityAtom, Activity),
    atom_string(TaskTokenAtom, TaskToken),
    faas:activity_heartbeat(MQType, ActivityAtom, TaskTokenAtom).

activity_heartbeated(Activity, TaskToken, HeartbeatSeconds) :-
    faasshell_mq(MQType),
    atom_string(ActivityAtom, Activity),
    atom_string(TaskTokenAtom, TaskToken),
    faas:activity_heartbeated(MQType, ActivityAtom, TaskTokenAtom,
                              HeartbeatSeconds).

event_publish(User, Event, Action) :-
    faasshell_mq(MQType),
    faas:event_publish(MQType, User, Event, Action).

event_published(User, Event, Action, Timeout) :-
    faasshell_mq(MQType),
    faas:event_published(MQType, User, Event, Action, Timeout).

event_subscribe(User, Event) :-
    faasshell_mq(MQType),
    faas:event_subscribe(MQType, User, Event).

event_subscribed(User, Event) :-
    faasshell_mq(MQType),
    faas:event_subscribed(MQType, User, Event).

%%
%%
:- multifile
       faas:mq_init/1,
       faas:activity_start/4,
       faas:activity_started/4,
       faas:activity_end/5,
       faas:activity_ended/5,
       faas:activity_heartbeat/3,
       faas:activity_heartbeated/4,
       faas:event_publish/4,
       faas:event_published/5,
       faas:event_subscribe/3,
       faas:event_subscribed/3.


%% activity task messaging
:- dynamic
       activity_task_queue/1.

faas:mq_init(built_in) :-
    message_queue_create(Q, [max_size(10)]),     % TODO: queue size
    assertz(activity_task_queue(Q)).

faas:activity_start(built_in, Activity, TaskToken, InputText) :-
    activity_task_queue(MQueue),
    thread_send_message(MQueue,
                        get_activity_task(Activity, TaskToken),
                        [timeout(60)]),
    thread_get_message(MQueue,
                       reply_activity_task(Activity, TaskToken, InputText),
                       [timeout(60)]).

faas:activity_started(built_in, Activity, InputText, TaskToken) :-
    activity_task_queue(MQueue),
    thread_get_message(MQueue, get_activity_task(Activity, TaskToken)),
    thread_send_message(MQueue,
                        reply_activity_task(Activity, TaskToken, InputText)).

faas:activity_end(built_in, Activity, TaskToken, Result, OutputText) :-
    activity_task_queue(MQueue),
    thread_send_message(MQueue,
                        send_task_result(Activity, TaskToken, Result,
                                         OutputText),
                        [timeout(60)]).

faas:activity_ended(built_in, Activity, TaskToken, Result, OutputText) :-
    activity_task_queue(MQueue),
    thread_get_message(MQueue,
                       send_task_result(Activity, TaskToken, Result,
                                        OutputText)).

faas:activity_heartbeat(built_in, Activity, TaskToken) :-
    activity_task_queue(MQueue),
    thread_send_message(MQueue,
                        send_task_heartbeat(Activity, TaskToken),
                        [timeout(60)]),
    thread_get_message(MQueue,
                       reply_task_heartbeat(Activity, TaskToken),
                       [timeout(60)]).

faas:activity_heartbeated(built_in, Activity, TaskToken, HeartbeatSeconds) :-
    activity_task_queue(MQueue),
    thread_get_message(MQueue, send_task_heartbeat(Activity, TaskToken),
                         [timeout(HeartbeatSeconds)])
    -> thread_send_message(MQueue,
                           reply_task_heartbeat(Activity, TaskToken)).

faas:event_publish(built_in, User, Event, Action) :-
    activity_task_queue(MQueue),
    debug(mq, 'faas:event_publish1(built_in, ~w, ~w, ~w)~n', [User, Event, Action]),
    thread_send_message(MQueue,
                        event_publish(User, Event, Action),
                        [timeout(60)]),
    debug(mq, 'faas:event_publish2(built_in, ~w, ~w, ~w)~n', [User, Event, Action]).

faas:event_published(built_in, User, Event, Action, Timeout) :-
    activity_task_queue(MQueue),
    debug(mq, 'faas:event_published1(built_in, ~w, ~w, ~w)~n',
          [User, Event, Action]),
    thread_get_message(MQueue,
                       event_publish(User, Event, Action), [timeout(Timeout)]),
    debug(mq, 'faas:event_published2(built_in, ~w, ~w, ~w)~n',
          [User, Event, Action]).

faas:event_subscribe(built_in, User, Event) :-
    activity_task_queue(MQueue),
    debug(mq, 'faas:event_subscribe1(built_in, ~w, ~w)~n', [User, Event]),
    thread_send_message(MQueue,
                        event_subscribe(User, Event),
                        [timeout(60)]),
    debug(mq, 'faas:event_subscribe2(built_in, ~w, ~w)~n', [User, Event]).

faas:event_subscribed(built_in, User, Event) :-
    activity_task_queue(MQueue),
    debug(mq, 'faas:event_subscribed1(built_in, ~w, ~w)~n', [User, Event]),
    thread_get_message(MQueue,
                        event_subscribe(User, Event)),
    debug(mq, 'faas:event_subscribed2(built_in, ~w, ~w)~n', [User, Event]).
