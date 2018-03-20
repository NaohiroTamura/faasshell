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

:- module(gcp_api_utils,
          [ gcp/1
         ]).

:- use_module(json_utils).
:- use_module(proxy_utils).

:- use_module(library(http/http_client)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(http/json)).

%%
%% $ swipl -q -l src/gcp_api_utils.pl \
%%         -g gcp_api_utils:debug_gcp -g 'gcp_api_utils:opt(Options)'
debug_gcp :- debug(gcp > user_error),
             debug(gcp_cache > user_error).

%%
:- dynamic
       cached_gcp_jwt/2. % cached_gcp_jwt(JWT, Time)

gcp(Options) :-
    ( cached_gcp_jwt(CachedOptions, Time),
      get_time(Now),
      Now-Time =< 3300 %% 55 mins, JWT expires in 60 mins
    )
    -> debug(gcp_cache, '~w~n', [cache_hit(CachedOptions, Time)]),
       Options = CachedOptions
    ;  ( retract(cached_gcp_jwt(CachedOptions, Time))
         -> debug(gcp_cache, '~w~n', [cache_retracted(CachedOptions,Time)])
         ;  debug(gcp_cache, '~w~n', [cache_not_exist(CachedOptions,Time)])
       ),
       debug(supress, '~w~n', [supress_compiler_warning(CachedOptions,Time)]),
       get_time(Renew),
       gcp_renew(Options),
       assertz(cached_gcp_jwt(Options, Renew)),
       debug(gcp_cache, '~w~n', [cache_asserted(Options, Renew)]).

gcp_renew(Options) :-
    proxy_utils:http_proxy('https://www.googleapis.com', ProxyOptions),
    debug(gcp, '~w~n', [proxy_options(ProxyOptions)]),
    ( getenv('GOOGLE_APPLICATION_CREDENTIALS', File)
      -> setup_call_cleanup(
                 open(File, read, S, []),
                 json_read_dict(S, CredDict, []),
                 close(S)),
         jwt(CredDict, JWT),
         access_token_request(JWT, Reply),
         debug(gcp, '~w~n', [token_type(Reply)]),
         atomics_to_string([Reply.token_type, Reply.access_token], ' ',
                           AuthorizationHeader),
         debug(gcp, '~w~n', [authorization_header(AuthorizationHeader)]),
         Options = [ request_header('Authorization'=AuthorizationHeader),
                     gcp_project(CredDict.project_id)
                   | ProxyOptions ]
      ;  Options = ProxyOptions
    ).

jwt(Cred, JWT) :-
    base64url('{"alg":"RS256","typ":"JWT"}', HeaderEnc),

    get_time(Now),
    IaT is round(Now),
    Exp is IaT + 3600,

    %% converting JSON Dict to String by atom_json_dict/3 inserts return
    %% code and space for visibility which hinders for gcp to parse JSON.
    /*
    Claim = _{iss: Cred.client_email,
              scope: 'https://www.googleapis.com/auth/cloud-platform',
              aud: 'https://www.googleapis.com/oauth2/v4/token',
              exp: Exp,
              iat: IaT
             },

    atom_json_dict(ClaimStr, Claim, []),
    */
    format(string(ClaimStr),
           '{"iss":"~w","scope":"~w","aud":"~w","exp":~w,"iat":~w}',
           [Cred.client_email,
            'https://www.googleapis.com/auth/cloud-platform',
            'https://www.googleapis.com/oauth2/v4/token',
            Exp,
            IaT
          ]),
    debug(gcp, '~w~n', [claim(ClaimStr)]),

    %% base64url/2 works fine for visible string
    %% In case, bytes_base64/2 code is left in comment here.
    /*string_codes(ClaimStr, ClaimCodes),
    bytes_base64(ClaimCodes, ClaimEnc),*/
    base64url(ClaimStr, ClaimEnc),
    debug(gcp, '~w~n', [claim_enc(ClaimEnc)]),

    atomic_list_concat([HeaderEnc, ClaimEnc], '.', Data),

    sha256_with_rsa(Cred.private_key, '', Data, Signature),
    debug(gcp, '~w~n', [sig(Signature)]),

    %% don't forget to convert to binary from hex string
    hex_bytes(Signature, Bytes),
    debug(gcp, '~w~n', [sig_bytes(Bytes)]),

    %% base64url/2 doesn't generate correct base64 for binary
    %% see the bottom of this file
    %% https://github.com/SWI-Prolog/swipl-devel/issues/253
    bytes_base64(Bytes, SignatureEnc),
    debug(gcp, '~w~n', [sig_enc(SignatureEnc)]),

    atomic_list_concat([HeaderEnc, ClaimEnc, SignatureEnc], '.', JWT).

sha256_with_rsa(PemKeyStr, Password, Data, Signature) :-
    Algorithm = sha256,
    read_key(PemKeyStr, Password, Key),
    debug(gcp, '~w~n', [key(Key)]),
    crypto_data_hash(Data, Hash, [algorithm(Algorithm),
                                  encoding(utf8)]),
    rsa_sign(Key, Hash, Signature, [type(Algorithm)]).

read_key(PemKeyStr, Password, Key) :-
    setup_call_cleanup(
    open_string(PemKeyStr, In),
        load_private_key(In, Password, Key),
        close(In)).

access_token_request(JWT, Reply) :-
    GrantType = 'urn:ietf:params:oauth:grant-type:jwt-bearer',
    uri_encoded(query_value, GrantType, GrantTypeEnc),
    uri_encoded(query_value, JWT, JWTEnc),
    atomic_list_concat(['grant_type=', GrantTypeEnc,
                        '&assertion=', JWTEnc], Payload),
    URL = 'https://www.googleapis.com/oauth2/v4/token',
    proxy_utils:http_proxy(URL, ProxyOptions),
    http_post(URL, atom('application/x-www-form-urlencoded',Payload), R,
              [ request_header('Accept'='application/json')
              | ProxyOptions
              ]),
    debug(gcp, '~w~n', [reply_term(R)]),
    json_utils:term_json_dict(R, R2),
    ( is_dict(R2)
      -> Reply = R2
      ;  atom_json_dict(R, Reply, [])
    ),
    debug(gcp, '~w~n', [reply_dict(Reply)]).

%%
%% work around until v7.7.10 is available
%% ADDED: base64_encoded/3 and base64_encoded//2, providing options
%% https://github.com/SWI-Prolog/swipl-devel/commit/173bcf66417b328d64957fe145f4246e58bb2452
%%
%% Please add Base64 conversion predicate without padding #253
%% https://github.com/SWI-Prolog/swipl-devel/issues/253
%% https://www.metalevel.at/base64.pl
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Bidirectional Bytes <-> Base64 conversion
   =========================================

   This implements Base64 conversion *without padding*.

   Examples (double_quotes set to codes):

       ?- bytes_base64("test", Bs).
       %@ Bs = dGVzdA .

       ?- bytes_base64(Ls, 'dGVzdA'),
          atom_codes(A, Ls).
       %@ Ls = [116, 101, 115, 116],
       %@ A = test .

   Written 2017 by Markus Triska (triska@metalevel.at)
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */


:- set_prolog_flag(double_quotes, codes).

:- use_module(library(clpfd)).

n_base64(0 , 'A'). n_base64(1 , 'B'). n_base64(2 , 'C'). n_base64(3 , 'D').
n_base64(4 , 'E'). n_base64(5 , 'F'). n_base64(6 , 'G'). n_base64(7 , 'H').
n_base64(8 , 'I'). n_base64(9 , 'J'). n_base64(10, 'K'). n_base64(11, 'L').
n_base64(12, 'M'). n_base64(13, 'N'). n_base64(14, 'O'). n_base64(15, 'P').
n_base64(16, 'Q'). n_base64(17, 'R'). n_base64(18, 'S'). n_base64(19, 'T').
n_base64(20, 'U'). n_base64(21, 'V'). n_base64(22, 'W'). n_base64(23, 'X').
n_base64(24, 'Y'). n_base64(25, 'Z'). n_base64(26, 'a'). n_base64(27, 'b').
n_base64(28, 'c'). n_base64(29, 'd'). n_base64(30, 'e'). n_base64(31, 'f').
n_base64(32, 'g'). n_base64(33, 'h'). n_base64(34, 'i'). n_base64(35, 'j').
n_base64(36, 'k'). n_base64(37, 'l'). n_base64(38, 'm'). n_base64(39, 'n').
n_base64(40, 'o'). n_base64(41, 'p'). n_base64(42, 'q'). n_base64(43, 'r').
n_base64(44, 's'). n_base64(45, 't'). n_base64(46, 'u'). n_base64(47, 'v').
n_base64(48, 'w'). n_base64(49, 'x'). n_base64(50, 'y'). n_base64(51, 'z').
n_base64(52, '0'). n_base64(53, '1'). n_base64(54, '2'). n_base64(55, '3').
n_base64(56, '4'). n_base64(57, '5'). n_base64(58, '6'). n_base64(59, '7').
n_base64(60, '8'). n_base64(61, '9'). n_base64(62, '+'). n_base64(63, '/').

bytes_base64(Ls, B) :-
        (   atom(B) ->
            atom_chars(B, Chars),
            maplist(n_base64, Is, Chars),
            phrase(bytes_base64_(Ls), Is),
            Ls ins 0..255
        ;   phrase(bytes_base64_(Ls), Is),
            Is ins 0..63,
            maplist(n_base64, Is, Bs),
            atomic_list_concat(Bs, B)
        ).

bytes_base64_([])         --> [].
bytes_base64_([A])        --> [W,X],
        { A #= W*4 + X//16,
          X #= 16*_ }.
bytes_base64_([A,B])      --> [W,X,Y],
        { A #= W*4 + X//16,
          B #= (X mod 16)*16 + Y//4,
          Y #= 4*_ }.
bytes_base64_([A,B,C|Ls]) --> [W,X,Y,Z],
        { A #= W*4 + X//16,
          B #= (X mod 16)*16 + Y//4,
          C #= (Y mod 4)*64 + Z },
        bytes_base64_(Ls).
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   Testing.
   ========

   Any cases where decoding does not yield the encoded list?

    ?- length(Ls, L),
       portray_clause(L),
       maplist(between(0,255), Ls),
       bytes_base64(Ls, B64),
       bytes_base64(Ls1, B64),
       dif(Ls, Ls1).
    %@ 0.
    %@ 1.
    %@ 2.

   Test case from:

   https://en.wikipedia.org/wiki/Base64

   A quote from Thomas Hobbes' Leviathan and its Base64 encoding.

   ?- leviathan_base64(Bytes, B64),
      bytes_base64(Bytes, B64).
   %@ Bytes = [77, 97, 110, 32, 105, 115, 32, 100, 105|...],
   %@ B64 = 'TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4' .
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/*
leviathan_base64(
"Man is distinguished, not only by his reason, but by this singular passion from \c
other animals, which is a lust of the mind, that by a perseverance of delight \c
in the continued and indefatigable generation of knowledge, exceeds the short \c
vehemence of any carnal pleasure.",
                'TWFuIGlzIGRpc3Rpbmd1aXNoZWQsIG5vdCBvbmx5IGJ5IGhpcyByZWFzb24sIGJ1dCBieSB0aGlzIHNpbmd1bGFyIHBhc3Npb24gZnJvbSBvdGhlciBhbmltYWxzLCB3aGljaCBpcyBhIGx1c3Qgb2YgdGhlIG1pbmQsIHRoYXQgYnkgYSBwZXJzZXZlcmFuY2Ugb2YgZGVsaWdodCBpbiB0aGUgY29udGludWVkIGFuZCBpbmRlZmF0aWdhYmxlIGdlbmVyYXRpb24gb2Yga25vd2xlZGdlLCBleGNlZWRzIHRoZSBzaG9ydCB2ZWhlbWVuY2Ugb2YgYW55IGNhcm5hbCBwbGVhc3VyZS4').
*/
