-module(serverless_logger).
-behaviour(pipe).
-compile({parse_transform, category}).
-include_lib("datum/include/datum.hrl").

-export([log/3, log_/3, resume/0, suspend/0]).
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
   events = undefined :: _
}).

%%-----------------------------------------------------------------------------
%%
%% api
%%
%%-----------------------------------------------------------------------------
log(Type, Pid, Msg) ->
   pipe:call(?MODULE, {os:timestamp(), Type, Pid, Msg}, infinity).

log_(Type, Pid, Msg) ->
   pipe:call(?MODULE, {os:timestamp(), Type, Pid, Msg}).
   % pipe:send(?MODULE, {os:timestamp(), Type, Pid, Msg}).

resume() ->
   pipe:call(?MODULE, resume, infinity).

suspend() ->
   pipe:call(?MODULE, suspend, infinity).

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
      Group  <- env("AWS_LAMBDA_LOG_GROUP_NAME"),
      Stream <- env("AWS_LAMBDA_LOG_STREAM_NAME"),
      cats:unit(handle,
         #state{
            group  = Group,
            stream = Stream,
            events = q:new()
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
handle({T, Type, Pid, Msg}, _, #state{events = Events} = State) ->
   io:fwrite(standard_error, message(Type, Pid, Msg), []),
   {reply, ok, State};
   % {reply, ok,
   %    State#state{
   %       events = q:enq(
   %          #{
   %             message => message(Type, Pid, Msg), 
   %             timestamp => milliseconds(T)
   %          }, 
   %          Events
   %       )
   %    }
   % };

handle(resume, _, #state{} = State) ->
   {reply, ok, State};

handle(suspend, _, #state{} = State) ->
   suspend(State),
   {reply, ok, State#state{events = q:new()}}.
  
%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

%%
env(Var) ->
   case os:getenv(Var) of
      false ->
         {ok, undefined};
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

message(Type, Pid, Msg)
 when is_map(Msg) ->
   erlang:iolist_to_binary(
      io_lib:format("[~s] ~p: ~s", [Type, Pid, jsx:encode(Msg)])
   );

message(Type, Pid, Msg) ->
   erlang:iolist_to_binary(
      io_lib:format("[~s] ~p: ~p", [Type, Pid, Msg])
   ).


%%
publish(#state{group = undefined, stream = undefined, events = Events}) ->
   lists:foreach(
      fun(#{message := Message}) -> 
         file:write(standard_error, Message)
      end,
      q:list(Events)
   );

publish(#state{group = Group, stream = Stream, events = Events}) ->
   [either ||
      Config <- erlcloud_aws:auto_config(),
      erlcloud_cloudwatch_logs:describe_log_streams(Group, Stream, Config),
      cats:unit(token(_, _)),
      erlcloud_cloudwatch_logs:put_logs_events(Group, Stream, _, q:list(Events), Config)
   ].  

%%
suspend(#state{events = ?queue()}) ->
   ok;
suspend(#state{} = State) ->
   case publish(State) of
      {error, {http_error, 400, _, Reason}} ->
         case jsx:decode(Reason, [return_maps]) of
            #{<<"__type">> := <<"InvalidSequenceTokenException">>} ->
               suspend(State);
            _ ->
               ok
         end;
      _ ->
         ok
   end.
