-module(helloworld).
-export([main/1]).

%%
%%
main(_) ->
   % io:format("==> ~p~n", [erlang:node()]).
   serverless:spawn(fun identity/1).

%%
%%
-spec identity(map()) -> datum:either(map()).

identity(Json) ->
   serverless:notice(#{spawn => helloworld}),
   serverless:notice(#{input => Json}),

   {ok, #{
      node => typecast:s(erlang:node()),
      helloworld => Json
   }}.

