%%
%% Copyright (C) 2018 Dmitry Kolesnikov
%%
%% This file may be modified and distributed under the terms
%% of the MIT license.  See the LICENSE file for details.
%% https://github.com/fogfish/serverless
%%
-module(serverless_mock_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-compile(export_all).

all() ->
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      NAry =:= 1
   ].

spawn_success(_) ->
   serverless:mock(
      serverless_test, 
      #{<<"do">> => <<"ok">>},
      #{<<"code">> => 200}
   ).

spawn_failure(_) ->
   serverless:mock(
      serverless_test, 
      #{<<"do">> => <<"fail">>}, 
      {'EXIT', fail}
   ).

spawn_error(_) ->
   serverless:mock(
      serverless_test, 
      #{<<"do">> => <<"error">>},
      {error, badarg}
   ).
