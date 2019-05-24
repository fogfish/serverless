-module(serverless_test).
-export([main/1]).

main(Opts) ->
   serverless:spawn(fun identity/1, Opts).


identity(#{<<"do">> := <<"ok">>}) ->
   {ok, #{<<"code">> => 200}};

identity(#{<<"do">> := <<"fail">>}) ->
   exit(fail);

identity(#{<<"do">> := <<"error">>}) ->
   {error, badarg}.
