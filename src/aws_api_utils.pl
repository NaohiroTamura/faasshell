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

%
% AWS Signature Version 4 Signing Process
%
:- module(aws_api_utils,
          [ aws_lambda/5
         ]).

lambda(list, 'GET', Function, Resource) :-
    atomic_list_concat(['/', Function], Resource).
lambda(invoke, 'POST', Function, Resource) :-
    atomic_list_concat(['/', Function, '/invocations'], Resource).
lambda(delete, 'DELETE', Function, Resource) :-
    atomic_list_concat(['/', Function], Resource).

aws_lambda(Action, ARN, RequestParameters, Payload, Options) :-
    getenv('AWS_ACCESS_KEY_ID', AWS_ACCESS_KEY_ID),
    getenv('AWS_SECRET_ACCESS_KEY', AWS_SECRET_ACCESS_KEY),
    atomic_list_concat([arn, aws, lambda, Region, _, function, Function], ':', ARN),
    lambda(Action, Method, Function, Resource),
    %% Region = 'us-east-2',
    Scheme = 'https',
    Service = 'lambda',
    Path = '/2015-03-31/functions',
    atomic_list_concat([Service, Region, 'amazonaws', 'com'], '.', Host),
    uri_components(Endpoint, uri_components(Scheme, Host, Path, _, _)),
    %%Endpoint = 'https://lambda.us-east-2.amazonaws.com/2015-03-31/functions',

    %% TASK 1: CREATE A CANONICAL REQUEST
    canonical_request(Method, Host, Path, Resource, RequestParameters, Payload,
                      AmzDate, DateStamp, CanonicalRequest, SignedHeaders),

    %% TASK 2: CREATE THE STRING TO SIGN
    string_to_sign(AmzDate, DateStamp, Region, Service, CanonicalRequest,
                   Algorithm, CredentialScope, StringToSign),

    %% TASK 3: CALCULATE THE SIGNATURE
    calc_signature(AWS_SECRET_ACCESS_KEY, DateStamp, Region, Service, StringToSign,
                   Signature),

    %% TASK 4: ADD SIGNING INFORMATION TO THE REQUEST
    auth_header(Algorithm, AWS_ACCESS_KEY_ID, CredentialScope, SignedHeaders,
                Signature, AuthorizationHeader),

    ( RequestParameters = ''
      -> atomics_to_string([Endpoint, Resource], URL)
      ;  atomics_to_string([Endpoint, Resource, '?', RequestParameters], URL)
    ),
    Options = [
        aws_access_key_id(AWS_ACCESS_KEY_ID),
        aws_secret_access_key(AWS_SECRET_ACCESS_KEY),
        url(URL),
        request_header('X-Amz-Date'=AmzDate),
        request_header('Authorization'=AuthorizationHeader)].

%% TASK 1: CREATE A CANONICAL REQUEST
canonical_request(Method, Host, Path, Resource, RequestParameters, Payload,
                  AmzDate, DateStamp, CanonicalRequest, SignedHeaders) :-
    amzdate(AmzDate, DateStamp),
    atom_concat(Path, Resource, CanonicalUri),
    CanonicalQuerystring = RequestParameters,
    format(string(CanonicalHeaders), 'host:~w~nx-amz-date:~w~n',[Host, AmzDate]),
    SignedHeaders = 'host;x-amz-date',
    sha256_hexdigest(Payload, PayloadHash),
    /* writeln(payload_hash(PayloadHash)),*/
    format(string(CanonicalRequest), '~w~n~w~n~w~n~w~n~w~n~w',
           [Method, CanonicalUri, CanonicalQuerystring, CanonicalHeaders,
            SignedHeaders, PayloadHash]).

%% TASK 2: CREATE THE STRING TO SIGN
string_to_sign(AmzDate, DateStamp, Region, Service, CanonicalRequest,
               Algorithm, CredentialScope, StringToSign) :-
    Algorithm = 'AWS4-HMAC-SHA256',
    atomic_list_concat([DateStamp, Region, Service, 'aws4_request'], '/',
                       CredentialScope),
    sha256_hexdigest(CanonicalRequest, CanonicalRequestHash),
    /* writeln(canonical_request(CanonicalRequest)),
       writeln(canonical_request_hash(CanonicalRequestHash)), */
    format(string(StringToSign), '~w~n~w~n~w~n~w',
           [Algorithm, AmzDate, CredentialScope, CanonicalRequestHash]).

%% TASK 3: CALCULATE THE SIGNATURE
calc_signature(AWS_SECRET_ACCESS_KEY, DateStamp, Region, Service, StringToSign,
               Signature) :-
    signature_key(AWS_SECRET_ACCESS_KEY, DateStamp, Region, Service, SigningKey),
    /* hex_bytes(HexSigningKey, SigningKey),
       writeln(signing_key(HexSigningKey)),
       writeln(string_to_sign(StringToSign)), */
    signature(SigningKey, StringToSign, Signature).
    /* writeln(signature(Signature)). */

%% TASK 4: ADD SIGNING INFORMATION TO THE REQUEST
auth_header(Algorithm, AWS_ACCESS_KEY_ID, CredentialScope, SignedHeaders, Signature,
            AuthorizationHeader) :-
    format(string(AuthorizationHeader),
           '~w Credential=~w/~w, SignedHeaders=~w, Signature=~w',
           [Algorithm, AWS_ACCESS_KEY_ID, CredentialScope, SignedHeaders,
            Signature]).

%%
sign(Key, Msg, Hmac) :-
    hmac_sha(Key, Msg, Hmac, [algorithm(sha256), encoding(octet)]).

signature_key(Key, DateStamp, RegionName, ServiceName, SignatureKey) :-
    atom_concat('AWS4', Key, AWS4Key),
    sign(AWS4Key, DateStamp, KeyDate),
    sign(KeyDate, RegionName, KeyRegion),
    sign(KeyRegion, ServiceName, KeyService),
    sign(KeyService, 'aws4_request', SignatureKey).

signature(Key, Msg, HexHmac) :-
    sign(Key, Msg, Hmac),
    hex_bytes(HexHmac, Hmac).

sha256_hexdigest(Msg, HexHash) :-
    sha_hash(Msg, Hash, [algorithm(sha256), encoding(utf8)]),
    hex_bytes(HexHash, Hash).

/* amzdate("20180113T094413Z", "20180113") :- !. */
amzdate(AmzDate, DateStamp) :-
    get_time(Now),
    stamp_date_time(Now, DateTime, 'UTC'),
    format_time(string(AmzDate), '%Y%m%dT%H%M%SZ', DateTime),
    format_time(string(DateStamp), '%Y%m%d', DateTime).
