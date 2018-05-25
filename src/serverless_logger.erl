-module(serverless_logger).
-behaviour(pipe).
-compile({parse_transform, category}).

-export([log/3, sync/0]).
-export([
   start_link/0,
   init/1,
   free/2,
   handle/3
]).

%%
-record(state, {
   group  = undefined :: _,
   stream = undefined :: _,
   token  = undefined :: _
}).

%%-----------------------------------------------------------------------------
%%
%% api
%%
%%-----------------------------------------------------------------------------
log(Type, Pid, Msg) ->
   pipe:send(?MODULE, {os:timestamp(), Type, Pid, Msg}).

sync() ->
   pipe:call(?MODULE, sync).

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
      Config <- erlcloud_aws:auto_config(),
      Group  <- env("AWS_LAMBDA_LOG_GROUP_NAME"),
      Stream <- env("AWS_LAMBDA_LOG_STREAM_NAME"),
      erlcloud_cloudwatch_logs:describe_log_streams(Group, Stream, Config),
      Token  =< token(_, _),
      cats:unit(handle, 
         #state{
            group  = Group,
            stream = Stream,
            token  = Token
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
handle({T, Type, Pid, Msg}, _, #state{group = Group, stream = Stream, token = Token} = State) ->
   case
      [either ||
         Config  <- erlcloud_aws:auto_config(),
         cats:unit([#{message => message(Type, Pid, Msg), timestamp => milliseconds(T)}]),
         erlcloud_cloudwatch_logs:put_logs_events(Group, Stream, Token, _, Config)
      ]
   of
      {ok, NextToken} ->
         {next_state, handle, State#state{token = NextToken}};

      Error ->
         {stop, Error, State}
   end;

handle(sync, Pipe, State) ->
   pipe:ack(Pipe, ok),
   {next_state, handle, State}.

%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

%%
env(Var) ->
   case os:getenv(Var) of
      false ->
         {error, {env, Var}};
      Value ->
         {ok, erlang:iolist_to_binary(Value)}
   end.

%%
token(Spec, _) ->
   lens:get(lens:c(lens:hd(), lens:pair(<<"uploadSequenceToken">>)), Spec).

%%
milliseconds({A, B, C}) ->
   (A * 1000000 + B) * 1000 + erlang:trunc(C / 1000).

%%
message(Type, Pid, [H | _] = Msg)
 when is_integer(H) ->
   erlang:iolist_to_binary(
      io_lib:format("[~s] ~p: ~s", [Type, Pid, Msg])
   );

message(Type, Pid, Msg) ->
   erlang:iolist_to_binary(
      io_lib:format("[~s] ~p: ~s", [Type, Pid, jsx:encode(Msg)])
   ).
