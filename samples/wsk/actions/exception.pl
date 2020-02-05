%% -*- mode: prolog; coding: utf-8; -*-
%%
%% Copyright 2017-2020 FUJITSU LIMITED
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

%% $ wsk action create exception exception.pl \
%%   --docker ${docker_image_prefix}swipl8action -i
%%
%% $ wsk action invoke exception -ir
%% {
%%    "errorMessage": "This is a custom error!",
%%    "errorType": "custom_error"
%% }
%%
%% $ wsk action invoke exception \
%%   -p error '"myCustomError(\"this is my custom error!\", 200)"' -ir
%% {
%%     "errorMessage": "this is my custom error!",
%%     "errorType": "myCustomError"
%% }

main(Arg, _Out) :-
    _{error: E} :< Arg
    -> term_string(T, E, []),
       throw(T)
    ;  throw('CustomError'("This is a custom error!", 200)).

%% openwhisk invoker treats non-200 code as a container failure,
%% and generates the following JSON invalid error.
%%
%% $ wsk action invoke hello_exception -ir
%% {
%%    "error": "The action did not produce a valid JSON response: {\"errorMessage\":\"This is a custom error!\", \"errorType\":\"custom_error\"}"
%% }
%%
%% main(_Arg, _Out) :-
%%    throw(custom_error("This is a custom error!", 502)).
