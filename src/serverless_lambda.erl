%% @doc
%%
-module(serverless_lambda).
-behaviour(pipe).
-compile({parse_transform, category}).

-export([
   start_link/1,
   init/1,
   free/2,
   handle/3,
   lifecycle/2
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
      spawn_link(
         fun() -> 
            loop(
               lifecycle(Host, Lambda),
               #{so => [
                  %% socket options (so) are mandatory
                  %% tune HTTP timeouts so that runtime api would not fail
                  {checkout_timeout, infinity}
               ,  {connect_timeout,  30000}
               ,  {recv_timeout,     infinity}
               ]}
            )
         end
      )
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
   try
      [_Result | State1] = Lambda(State0),
      loop(Lambda, State1)
   catch _:Reason ->
      serverless_logger:log(emergency, self(), Reason),
      serverless_logger:log(emergency, self(), erlang:get_stacktrace()),
      exit(Reason)
   end.

%%
lifecycle(Host, Lambda) ->
   [m_state ||
      queue(Host),
      cats:unit( exec(Lambda, _) ),
      finalise(Host, _)
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

finalise(Host, {error, RequestId, #{} = Reason}) ->
   [m_http ||
      _ > "POST " ++ Host ++ "/runtime/invocation/" ++ RequestId ++ "/error",
      _ > "Content-Type: application/json",
      _ > "Connection: keep-alive",
      _ > Reason,

      _ < 202
   ];

finalise(Host, {error, RequestId, Reason}) ->
   [m_http ||
      _ > "POST " ++ Host ++ "/runtime/invocation/" ++ RequestId ++ "/error",
      _ > "Content-Type: text/plain",
      _ > "Connection: keep-alive",
      _ > erlang:iolist_to_binary(io_lib:format("[~s] ~p", [RequestId, Reason])),

      _ < 202
   ].

%%
exec(Lambda, {RequestId, Json}) ->
   Self = self(),
   {Pid, Ref} = erlang:spawn_opt(
      fun() ->
         case Lambda(Json) of
            {ok, Result} ->
               Self ! {ok, Result};
            {error, Reason} ->
               serverless_logger:log(critical, self(), Reason),
               Self ! {error, Reason};
            ok  ->
               {ok, undefined};
            Any ->
               {ok, Any}
         end
      end,
      [monitor]
   ),
   receive
      {ok, Result} ->
         {ok, RequestId, Result};
      {error, Reason} ->
         {error, RequestId, Reason};
      {'DOWN', Ref, process, Pid, Reason} ->
         serverless_logger:log(error, self(), Reason),
         {error, RequestId, Reason}
   end.
