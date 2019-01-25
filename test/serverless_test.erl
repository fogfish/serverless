-module(serverless_test).
-export([main/1]).

main(_) ->
   serverless:spawn(fun identity/1).


identity(#{<<"do">> := <<"ok">>}) ->
   {ok, #{<<"code">> => 200}};

identity(#{<<"do">> := <<"fail">>}) ->
   exit(fail);

identity(#{<<"do">> := <<"error">>}) ->
   {error, badarg}.
