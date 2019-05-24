-module(helloworld).
-export([main/1]).

%%
%%
main(Opts) ->
   serverless:spawn(fun identity/1, Opts).

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

