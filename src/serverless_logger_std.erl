%% @doc
%%
-module(serverless_logger_std).
-behaviour(gen_event).

-export([
   start/0,
   stop/0,
   init/1,
   terminate/2,
   handle_event/2,
   handle_call/2
]).


start() ->
   error_logger:tty(false),
   error_logger:add_report_handler(?MODULE).

stop() ->
   error_logger:delete_report_handler(?MODULE).

init(_) ->
   {ok, undefined}.

terminate(_, _) ->
   ok.

handle_event({error_report, _, {Pid, Type, Msg}} = X, State) ->
   log(Type, Pid, Msg),
   {ok, State};

handle_event({warning_report, _, {Pid, Type, Msg}}, State) ->
   log(Type, Pid, Msg),
   {ok, State};

handle_event({info_report, _, {Pid, Type, Msg}}, State) ->
   log(Type, Pid, Msg),
   {ok, State};

handle_event(_, State) ->
   {ok, State}.

handle_call(_, State) ->
   {noreply, State}.

log(emergency, Pid, Msg) ->
   serverless_logger:log(emergency, Pid, Msg);

log(alert, Pid, Msg) ->
   serverless_logger:log(alert, Pid, Msg);

log(critical, Pid, Msg) ->
   serverless_logger:log(critical, Pid, Msg);

log(error, Pid, Msg) ->
   serverless_logger:log(error, Pid, Msg);

log(warning, Pid, Msg) ->
   serverless_logger:log(warning, Pid, Msg);

log(notice, Pid, Msg) ->
   serverless_logger:log(notice, Pid, Msg);

log(info, Pid, Msg) ->
   serverless_logger:log(info, Pid, Msg);

log(debug, Pid, Msg) ->
   serverless_logger:log(debug, Pid, Msg);

log(_, _, _) ->
   ok.




