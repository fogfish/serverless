%% @doc
%%
-module(serverless).
-compile({parse_transform, category}).

-export([
   spawn/1,

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
   supervisor:start_child(serverless_sup,
      {serverless_lambda,  
         {serverless_lambda, start_link, [Lambda]}, 
         permanent, 
         10, 
         worker, 
         dynamic
      }
   ).

%%%------------------------------------------------------------------
%%%
%%% error logger
%%%
%%%------------------------------------------------------------------

%%
%% system us unusable
emergency(Msg) ->
   error_logger:error_report(emergency, Msg).

%%
%% action must be taken immediately
alert(Msg) ->
   error_logger:error_report(alert, Msg).

%%
%% 
critical(Msg) ->
   error_logger:error_report(critical, Msg).

%%
%% 
error(Msg) ->
   error_logger:error_report(error, Msg).

%%
%% 
warning(Msg) ->
   error_logger:warning_report(warning, Msg).

%%
%% normal but significant conditions
notice(Msg) ->
   error_logger:warning_report(notice, Msg).

%%
%% informational messages
info(Msg) ->
   error_logger:info_report(info, Msg).

%%
%% debug-level messages
debug(Msg) ->
   error_logger:info_report(debug, Msg).

