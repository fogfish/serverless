-module(serverless_logger).
-behaviour(pipe).
-compile({parse_transform, category}).
-include_lib("datum/include/datum.hrl").

-export([log/3]).
-export([
   start_link/0,
   init/1,
   free/2,
   handle/3
]).

%%
-record(state, {crlf}).

%%-----------------------------------------------------------------------------
%%
%% api
%%
%%-----------------------------------------------------------------------------
log(Type, Pid, Msg) ->
   pipe:call(?MODULE, {os:timestamp(), Type, Pid, Msg}, infinity).

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
      cats:unit(handle, 
         #state{
            crlf = application:get_env(serverless, crlf, <<$\r>>)
         }
      )
   ].

free(_, _) ->
   serverless_logger_std:stop().

%%-----------------------------------------------------------------------------
%%
%% state machine
%%
%%-----------------------------------------------------------------------------
handle({_T, Type, Pid, Msg}, _, #state{crlf = CRLF} = State) ->
   io:fwrite(standard_error, message(Type, Pid, Msg, CRLF), []),
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
message(_Type, _Pid, $-, CRLF) ->
   <<CRLF/binary, "-----------------------------------------------------------------------------", CRLF/binary, CRLF/binary>>;

message(Type, Pid, [H | _] = Msg, CRLF)
 when is_integer(H) ->
   erlang:iolist_to_binary(
      io_lib:format("[~s] ~p: ~s~s", [Type, Pid, Msg, CRLF])
   );

message(Type, Pid, Msg, CRLF)
 when is_map(Msg) ->
   erlang:iolist_to_binary(
      io_lib:format("[~s] ~p:~s~s~n", [Type, Pid, CRLF, 
         jsx:format(jsx:encode(Msg), [space, {indent, 2}, {newline, CRLF}])
      ])
   );

message(Type, Pid, Msg, CRLF) ->
   erlang:iolist_to_binary(
      io_lib:format("[~s] ~p: ~p~s", [Type, Pid, Msg, CRLF])
   ).
