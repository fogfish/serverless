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

%%-----------------------------------------------------------------------------
%%
%% factory
%%
%%-----------------------------------------------------------------------------
start_link(Lambda) ->
   pipe:start_link({local, ?MODULE}, ?MODULE, [Lambda], []).

init([Lambda]) ->
   {ok, handle, 
      spawn_link(fun() -> loop(Lambda) end)
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

loop(Lambda) ->
   [either ||
      recv(),
      exec(Lambda, _),
      send(_),
      loop(Lambda)
   ].

%%
recv() ->
   case file:read_line(standard_io) of
      {ok, Json} ->
         {ok, jsx:decode(Json, [return_maps])};
      {error, _} = Error ->
         Error;
      eof ->
         {error, eof}
   end.

%%
send(undefined) ->
   ok;
send(Json) ->
   [either ||
      cats:unit(jsx:encode(Json)),
      file:write(standard_io, _)
   ].

%%
exec(Lambda, In) ->
   case Lambda(In) of
      {ok, _} = Result ->
         Result;
      {error, _} = Error ->
         Error;
      ok  ->
         {ok, undefined};
      Any ->
         {ok, Any}
   end.
