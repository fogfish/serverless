-module(serverless_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([
   spawn_success/1
,  spawn_failure/1
,  spawn_error/1
]).

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
   try
      serverless:mock(
         serverless_test, 
         #{<<"do">> => <<"fail">>}, 
         #{}
      ),
      exit(unhandled)
   catch _:_ ->
      ok
   end.

spawn_error(_) ->
   serverless:mock(
      serverless_test, 
      #{<<"do">> => <<"error">>},
      undefined
   ).
