-module(serverless_logger).
-behaviour(pipe).
-compile({parse_transform, category}).

-export([log/3, resume/0, suspend/0]).
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
   pipe:send(?MODULE, {os:timestamp(), Type, Pid, Msg}).

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
   {next_state, handle,
      State#state{
         events = q:enq(
            #{
               message => message(Type, Pid, Msg), 
               timestamp => milliseconds(T)
            }, 
            Events
         )
      }
   };

handle(resume, _, #state{} = State) ->
   {reply, ok, State};

handle(suspend, _, #state{group = Group, stream = Stream, events = Events} = State) ->
   [either ||
      Config <- erlcloud_aws:auto_config(),
      erlcloud_cloudwatch_logs:describe_log_streams(Group, Stream, Config),
      cats:unit(token(_, _)),
      erlcloud_cloudwatch_logs:put_logs_events(Group, Stream, _, q:list(Events), Config)
   ],
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
