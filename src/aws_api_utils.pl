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

/*:- module(aws_api_utils,
          [ openwhisk/1,
            api_url/4,
            api_action_name/3
         ]).
*/

:- use_module(json_utils).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).

aws_lambda(Method, Resource, RequestParameters, Payload, Options) :-
    getenv('AWS_ACCESS_KEY_ID', AWS_ACCESS_KEY_ID),
    getenv('AWS_SECRET_ACCESS_KEY', AWS_SECRET_ACCESS_KEY),
    %% Service = 'lambda',
    %% Host = 'lambda.us-east-2.amazonaws.com',
    %% Region = 'us-east-2',
    Endpoint = 'https://lambda.us-east-2.amazonaws.com/2015-03-31/functions',
    uri_components(Endpoint, 
                   uri_components(Scheme, Host, Path, _Search, _Fragment)),
    atomic_list_concat([Service, Region |_], '.', Host),

    %% ************* TASK 1: CREATE A CANONICAL REQUEST *************
    amzdate(AmzDate),
    atom_concat('/2015-03-31/functions', Resource, CanonicalUri),
    CanonicalQuerystring = RequestParameters,
    format(string(CanonicalHeaders), 'host:~w~nx-amz-date:~w~n',[Host, AmzDate]),
    SignedHeaders = 'host;x-amz-date',
    sha256_hexdigest(Payload, PayloadHash),
    /* writeln(payload_hash(PayloadHash)),*/
    format(string(CanonicalRequest), '~w~n~w~n~w~n~w~n~w~n~w',
           [Method, CanonicalUri, CanonicalQuerystring, CanonicalHeaders,
            SignedHeaders, PayloadHash]),

    %% ************* TASK 2: CREATE THE STRING TO SIGN *************
    datestamp(DateStamp),
    Algorithm = 'AWS4-HMAC-SHA256',
    atomic_list_concat([DateStamp, Region, Service, 'aws4_request'], '/',
                       CredentialScope),
    sha256_hexdigest(CanonicalRequest, CanonicalRequestHash),
    /* writeln(canonical_request(CanonicalRequest)),
       writeln(canonical_request_hash(CanonicalRequestHash)), */
    format(string(StringToSign), '~w~n~w~n~w~n~w',
           [Algorithm, AmzDate, CredentialScope, CanonicalRequestHash]),

    %% ************* TASK 3: CALCULATE THE SIGNATURE *************
    signature_key(AWS_SECRET_ACCESS_KEY, DateStamp, Region, Service, SigningKey),
    /* hex_bytes(HexSigningKey, SigningKey),
       writeln(signing_key(HexSigningKey)),
       writeln(string_to_sign(StringToSign)), */
    signature(SigningKey, StringToSign, Signature),
    /* writeln(signature(Signature)), */

    %% ************* TASK 4: ADD SIGNING INFORMATION TO THE REQUEST *************
    format(string(AuthorizationHeader),
           '~w Credential=~w/~w, SignedHeaders=~w, Signature=~w',
           [Algorithm, AWS_ACCESS_KEY_ID, CredentialScope, SignedHeaders,
            Signature]),

    Options = [
        scheme(Scheme), host(Host), path(Path), service(Service), region(Region),
        aws_access_key_id(AWS_ACCESS_KEY_ID),
        aws_secret_access_key(AWS_SECRET_ACCESS_KEY),
        endpoint(Endpoint), search(RequestParameters),
        amzdate(AmzDate), authorization(AuthorizationHeader)].
        
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

datestamp(DateStamp) :-
    get_time(Now),
    stamp_date_time(Now, DateTime, 'UTC'),
    format_time(string(DateStamp), '%Y%m%d', DateTime).

/* amzdate("20180113T094413Z") :- !. */
amzdate(AmzDate) :-
    get_time(Now),
    stamp_date_time(Now, DateTime, 'UTC'),
    format_time(string(AmzDate), '%Y%m%dT%H%M%SZ', DateTime).

lambda_list(Reply, Code) :-
    %% ************* SEND THE REQUEST *************
    aws_lambda('GET', '', 'FunctionVersion=ALL', '', Options),
    option(amzdate(AmzDate), Options),
    option(authorization(AuthorizationHeader), Options),
    option(endpoint(Endpoint), Options),
    option(search(RequestParameters), Options),
    atomics_to_string([Endpoint, '?', RequestParameters], URL),
    /* writeln(url(URL)),
       writeln(header(amzdate(AmzDate), authorization(AuthorizationHeader))), */
    http_get(URL, R1,
             [status_code(Code),
              request_header('X-Amz-Date'=AmzDate),
              request_header('Authorization'=AuthorizationHeader),
              cert_verify_hook(cert_accept_any)]),
    json_utils:term_json_dict(R1, Reply).

lambda_hello(Reply, Code) :-
    %% ************* SEND THE REQUEST *************
    Resource = '/helloworld/invocations',
    Payload = '{"key1":"1","key2":"2","key3":"3","key4":"4"}',
    aws_lambda('POST', Resource, '', Payload, Options),
    option(amzdate(AmzDate), Options),
    option(authorization(AuthorizationHeader), Options),
    option(endpoint(Endpoint), Options),
    atomics_to_string([Endpoint, Resource], URL),
    /* writeln(url(URL)),
    writeln(header(amzdate(AmzDate), authorization(AuthorizationHeader))), */
    http_post(URL, atom('application/json', Payload), R1,
             [status_code(Code),
              request_header('X-Amz-Date'=AmzDate),
              request_header('Authorization'=AuthorizationHeader),
              cert_verify_hook(cert_accept_any)]),
    ( atomic(R1)
      -> Reply = R1
      ;  json_utils:term_json_dict(R1, Reply)
    ).
