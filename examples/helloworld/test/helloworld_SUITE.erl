-module(helloworld_SUITE).

-export([all/0]).
-export([hw/1]).

all() ->
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

hw(_) ->
   serverless:mock(helloworld, #{}, #{<<"helloworld">> => #{}}).
