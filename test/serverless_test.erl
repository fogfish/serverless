%%
%% Copyright (C) 2018 Dmitry Kolesnikov
%%
%% This file may be modified and distributed under the terms
%% of the MIT license.  See the LICENSE file for details.
%% https://github.com/fogfish/serverless
%%
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
