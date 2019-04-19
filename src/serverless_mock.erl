%% @doc
%%   mock serverless environment
-module(serverless_mock).
-include_lib("common_test/include/ct.hrl").

-export([
   test/3
]).

%%
%%
test(Lambda, Mock, Expect) ->
   meck:new(serverless, [passthrough]),
   meck:expect(serverless, spawn,
      fun(Fun) ->
         case Fun(Mock) of
            {ok, Expect} ->
               ok;
            Expect ->
               ok
         end 
      end
   ),
   Lambda:main([]),
   meck:unload(serverless).
