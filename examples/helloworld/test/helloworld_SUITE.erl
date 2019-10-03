%%
%% Copyright (C) 2018 Dmitry Kolesnikov
%%
%% This file may be modified and distributed under the terms
%% of the MIT license.  See the LICENSE file for details.
%% https://github.com/fogfish/serverless
%%
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
   #{
      helloworld := #{},
      node := <<"nonode@nohost">>
   } = serverless:mock(helloworld, #{}).
