-module(serverless_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([
   lifecycle/1
]).

all() ->
   [
      lifecycle
   ].

lifecycle(_) ->
   Message = <<"{\"a\":1}">>,
   meck:new(file, [unstick, passthrough]),
   meck:expect(file, read_line, fun(_) -> {ok, Message} end),
   meck:expect(file, write, fun(_, Message) -> ok end),

   ok = serverless:once(fun(Message) -> {ok, Message} end),

   true = meck:validate(file),
   meck:unload(file).
