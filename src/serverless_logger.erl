-module(serverless_logger).
-behaviour(pipe).
-compile({parse_transform, category}).
-include_lib("datum/include/datum.hrl").

-export([log/3, log_/3]).
-export([
   start_link/0,
   init/1,
   free/2,
   handle/3
]).

%%
-record(state, {}).

%%-----------------------------------------------------------------------------
%%
%% api
%%
%%-----------------------------------------------------------------------------
log(Type, Pid, Msg) ->
   pipe:call(?MODULE, {os:timestamp(), Type, Pid, Msg}, infinity).

log_(Type, Pid, Msg) ->
   pipe:call(?MODULE, {os:timestamp(), Type, Pid, Msg}, infinity).
   % pipe:send(?MODULE, {os:timestamp(), Type, Pid, Msg}).

%%-----------------------------------------------------------------------------
%%
%% factory
%%
%%-----------------------------------------------------------------------------
start_link() ->
   pipe:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
   [either ||
      cats:unit( erlang:process_flag(trap_exit, true) ),
      serverless_logger_std:start(),
      cats:unit(handle, #state{})
   ].

free(_, _) ->
   serverless_logger_std:stop().

%%-----------------------------------------------------------------------------
%%
%% state machine
%%
%%-----------------------------------------------------------------------------
handle({T, Type, Pid, Msg}, _, #state{} = State) ->
   io:fwrite(standard_error, message(Type, Pid, Msg), []),
   {reply, ok, State};

handle(resume, _, #state{} = State) ->
   {reply, ok, State};

handle(suspend, _, #state{} = State) ->
   {reply, ok, State}.
  
%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

%%
milliseconds({A, B, C}) ->
   (A * 1000000 + B) * 1000 + erlang:trunc(C / 1000).

%%
message(Type, Pid, [H | _] = Msg)
 when is_integer(H) ->
   erlang:iolist_to_binary(
      io_lib:format("[~s] ~p: ~s~n", [Type, Pid, Msg])
   );

message(Type, Pid, Msg)
 when is_map(Msg) ->
   erlang:iolist_to_binary(
      io_lib:format("[~s] ~p: ~s~n", [Type, Pid, jsx:encode(Msg)])
   );

message(Type, Pid, Msg) ->
   erlang:iolist_to_binary(
      io_lib:format("[~s] ~p: ~p~n", [Type, Pid, Msg])
   ).
