-module(helloworld).

-export([main/1]).


%%
%%
main(_) ->
   application:ensure_all_started(erlcloud),
   serverless:start(fun identity/1).   


%%
%%
-spec identity(map()) -> datum:either(map()).

identity(Json) ->
   {ok, #{helloworld => Json}}.
