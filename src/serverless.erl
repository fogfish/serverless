%% @doc
%%
-module(serverless).
-compile({parse_transform, category}).

-export([
   spawn/1,
   
   %% logger api
   emergency/1,
   alert/1,
   critical/1,
   error/1,
   warning/1,
   notice/1,
   info/1,
   debug/1
]).

%%%------------------------------------------------------------------
%%%
%%% api
%%%
%%%------------------------------------------------------------------
spawn(Lambda) ->
   {ok, _} = application:ensure_all_started(serverless, permanent),
   {ok, _} = serverless_sup:spawn(Lambda),
   spawn_loop().

spawn_loop() ->
   receive _ -> spawn_loop() end.

%%%------------------------------------------------------------------
%%%
%%% error logger
%%%
%%%------------------------------------------------------------------

%%
%% system us unusable
emergency(Msg) ->
   serverless_logger:log(emergency, self(), Msg).

%%
%% action must be taken immediately
alert(Msg) ->
   serverless_logger:log(alert, self(), Msg).

%%
%% 
critical(Msg) ->
   serverless_logger:log(critical, self(), Msg).

%%
%% 
error(Msg) ->
   serverless_logger:log(error, self(), Msg).

%%
%% 
warning(Msg) ->
   serverless_logger:log(warning, self(), Msg).

%%
%% normal but significant conditions
notice(Msg) ->
   serverless_logger:log(notice, self(), Msg).

%%
%% informational messages
info(Msg) ->
   serverless_logger:log(info, self(), Msg).

%%
%% debug-level messages
debug(Msg) ->
   serverless_logger:log(debug, self(), Msg).
