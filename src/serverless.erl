%% @doc
%%
-module(serverless).
-compile({parse_transform, category}).

-export([start/1]).

start(Fun) ->
   [either ||
      io:setopts([binary]),
      loop(Fun)
   ].

loop(Fun) ->
   [either ||
      recv(),
      Fun(_),
      send(_),
      loop(Fun)
   ].

recv() ->
   [either ||
      file:read_line(standard_io),
      cats:unit( jsx:decode(_, [return_maps]) )
   ].

send(undefined) ->
   ok;
send(Json) ->
   [either ||
      cats:unit(jsx:encode(Json)),
      file:write(standard_io, _)
   ].