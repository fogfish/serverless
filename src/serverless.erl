%% @doc
%%
-module(serverless).
-compile({parse_transform, category}).

-export([
   start/1,
   once/1
]).

%%
%%
-spec start(_) -> ok.

start(Fun) ->
   [either ||
      io:setopts([binary]),
      loop(Fun)
   ].

%%
%%
-spec once(_) -> ok.

once(Fun) ->
   [either ||
      io:setopts([binary]),
      recv(),
      Fun(_),
      send(_)
   ].

%%
loop(Fun) ->
   [either ||
      recv(),
      Fun(_),
      send(_),
      loop(Fun)
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