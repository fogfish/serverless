%%
%% Copyright (C) 2018 Dmitry Kolesnikov
%%
%% This file may be modified and distributed under the terms
%% of the MIT license.  See the LICENSE file for details.
%% https://github.com/fogfish/serverless
%%
-module(serverless_sup).
-behaviour(supervisor).

-export([
   start_link/0,
   init/1,
   spawn/1
]).

%%
-define(CHILD(Type, I),            {I,  {I, start_link,   []}, permanent, 10, Type, dynamic}).
-define(CHILD(Type, I, Args),      {I,  {I, start_link, Args}, permanent, 10, Type, dynamic}).
-define(CHILD(Type, ID, I, Args),  {ID, {I, start_link, Args}, permanent, 10, Type, dynamic}).

%%-----------------------------------------------------------------------------
%%
%% supervisor
%%
%%-----------------------------------------------------------------------------

start_link() ->
   supervisor:start_link({local, ?MODULE}, ?MODULE, []).
   
init([]) ->   
   {ok,
      {
         {one_for_one, 4, 300},
         [
            ?CHILD(worker, serverless_logger)
         ]
      }
   }.

%%
spawn(Lambda) ->
   supervisor:start_child(?MODULE, ?CHILD(worker, serverless_lambda, [Lambda])).
