%%
%% Copyright (C) 2018 Dmitry Kolesnikov
%%
%% This file may be modified and distributed under the terms
%% of the MIT license.  See the LICENSE file for details.
%% https://github.com/fogfish/serverless
%%
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
