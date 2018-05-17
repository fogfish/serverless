-module(serverless_app).
-behaviour(application).

-export([start/2, stop/1]).

%%
%%
start(_Type, _Args) ->
   io:setopts([binary]),
   serverless_sup:start_link().

%%
%%
stop(_State) ->
   ok.
