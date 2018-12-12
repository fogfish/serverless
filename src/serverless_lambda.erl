%% @doc
%%
-module(serverless_lambda).
-behaviour(pipe).
-compile({parse_transform, category}).

-export([
   start_link/1,
   init/1,
   free/2,
   handle/3
]).

-define(PROTOCOL, "http://").
-define(VERSION,  "/2018-06-01").

%%-----------------------------------------------------------------------------
%%
%% factory
%%
%%-----------------------------------------------------------------------------
start_link(Lambda) ->
   pipe:start_link({local, ?MODULE}, ?MODULE, [Lambda], []).

init([Lambda]) ->
   Host = ?PROTOCOL ++ os:getenv("AWS_LAMBDA_RUNTIME_API", "127.0.0.1:8888") ++ ?VERSION,
   {ok, handle, 
      spawn_link(fun() -> loop(lifecycle(Host, Lambda), #{}) end)
   }.

free(_, _) ->
   ok.

handle(_, _, State) ->
   {next_state, handle, State}.


%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

loop(Lambda, State0) ->
   [Result | State1] = Lambda(State0),
   loop(Lambda, State1).

%%
lifecycle(Host, Lambda) ->
   [m_state ||
      Json <- queue(Host),
      cats:unit( resume() ),
      cats:unit( exec(Lambda, Json) ),
      finalise(Host, _),
      cats:unit( suspend() )
   ].

%%
queue(Host) ->
   [m_http ||
      _ > "GET " ++ Host ++ "/runtime/invocation/next",
      _ > "Accept: application/json",
      _ > "Connection: keep-alive",

      _ < 200,
      ReqId < "Lambda-Runtime-Aws-Request-Id: _", 
      Json < '*',
      cats:unit({erlang:binary_to_list(ReqId), Json})
   ].

%%
finalise(Host, {ok, RequestId, Json}) ->
   [m_http ||
      _ > "POST " ++ Host ++ "/runtime/invocation/" ++ RequestId ++ "/response",
      _ > "Content-Type: application/json",
      _ > "Connection: keep-alive",
      _ > Json,

      _ < 202
   ];

finalise(Host, {error, RequestId, Reason}) ->
   [m_http ||
      _ > "POST " ++ Host ++ "/runtime/invocation/" ++ RequestId ++ "/error",
      _ > "Content-Type: text/plain",
      _ > "Connection: keep-alive",
      _ > erlang:iolist_to_binary(io_lib:format("[~s] ~p: ~p", [Reason])),

      _ < 202
   ].

%%
exec(Lambda, {RequestId, Json}) ->
   %% TODO: spawn a new process per invocation
   case Lambda(Json) of
      {ok, Result} ->
         {ok, RequestId, Result};
      {error, Reason} = Error ->
         serverless_logger:log(critical, self(), Error),
         {error, RequestId, Reason};
      ok  ->
         {ok, {RequestId, <<>>}};
      Any ->
         {ok, {RequestId, Any}}
   end.

%%
resume() ->
   serverless_logger:resume().

%%
suspend() ->
   serverless_logger:suspend().
