%%
%% Copyright (C) 2018 Dmitry Kolesnikov
%%
%% This file may be modified and distributed under the terms
%% of the MIT license.  See the LICENSE file for details.
%% https://github.com/fogfish/serverless
%%
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

