%%
%% Copyright (C) 2018 Dmitry Kolesnikov
%%
%% This file may be modified and distributed under the terms
%% of the MIT license.  See the LICENSE file for details.
%% https://github.com/fogfish/serverless
%%
%%
%% @doc
%%   mock serverless environment
-module(serverless_mock).
-include_lib("common_test/include/ct.hrl").

-export([
   test/3
]).

%%
%%
test(Lambda, Mock, Timeout) ->
   application:ensure_all_started(serverless),
   meck:new(serverless, [passthrough]),
   meck:new(serverless_logger, [passthrough]),

   meck:expect(serverless_logger, log,
      fun(Type, Pid, Msg) ->
         ct:pal("[~s]: ~p ~p", [Type, Pid, Msg])
      end
   ),

   Ref = erlang:make_ref(),
   Self = self(),
   meck:expect(serverless, spawn,
      fun(Fun, _) ->
         case (catch Fun(Mock)) of
            {ok, Value} ->
               Self ! {Ref, Value};
            Value ->
               Self ! {Ref, Value}
         end
      end
   ),
   Lambda:main([]),
   Result = receive
      {Ref, Value} ->
         Value
      after Timeout ->
         exit(timeout)
   end,

   meck:unload(serverless_logger),
   meck:unload(serverless),
   
   Result.
