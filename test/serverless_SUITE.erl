-module(serverless_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0]).
-export([
   spawn/1,
   lambda/1,
   logger/1
]).

all() ->
   [
      spawn,
      lambda,
      logger
   ].


spawn(_) ->
   setup_env(),
   setup_erlcloud_cloudwatch_logs(),

   erlang:spawn_link(
      fun() ->
         serverless:spawn(fun(_) -> ok end)
      end
   ),
   timer:sleep(100),
   application:stop(serverless),

   1  = meck:num_calls(erlcloud_cloudwatch_logs, describe_log_streams, '_'),
   true = meck:validate(erlcloud_cloudwatch_logs),

   unset_erlcloud_cloudwatch_logs(),
   unset_env().


lambda(_) ->
   setup_io(),

   {ok, _} = serverless_lambda:start_link(
      fun(X) -> 
         {ok, X} 
      end
   ),
   timer:sleep(100),

   true = meck:num_calls(file, read_line, '_') > 0,
   true = meck:num_calls(file, write, '_') > 0,
   true = meck:validate(file),

   unset_io().


logger(_) ->
   setup_env(),
   setup_erlcloud_cloudwatch_logs(),

   {ok, _} = serverless_logger:start_link(),
   serverless:emergency(x),
   serverless:alert([{a, b}]),
   serverless:critical("xxx"),
   serverless:error(<<"xxx">>),
   serverless:warning(x),
   serverless:notice(x),
   serverless:info(x),
   serverless:debug(x),
   timer:sleep(100),

   1 = meck:num_calls(erlcloud_cloudwatch_logs, describe_log_streams, '_'),
   8 = meck:num_calls(erlcloud_cloudwatch_logs, put_logs_events, '_'),
   true = meck:validate(erlcloud_cloudwatch_logs),

   unset_erlcloud_cloudwatch_logs(),
   unset_env().

%%-----------------------------------------------------------------------------
%%
%% private
%%
%%-----------------------------------------------------------------------------

%%
setup_env() ->
   true = os:putenv("AWS_ACCESS_KEY_ID", "access"),
   true = os:putenv("AWS_SECRET_ACCESS_KEY", "secret"),
   true = os:putenv("AWS_LAMBDA_LOG_GROUP_NAME",  "group"),
   true = os:putenv("AWS_LAMBDA_LOG_STREAM_NAME", "stream").

unset_env() ->
   os:unsetenv("AWS_ACCESS_KEY_ID"),
   os:unsetenv("AWS_SECRET_ACCESS_KEY"),
   os:unsetenv("AWS_LAMBDA_LOG_GROUP_NAME"),
   os:unsetenv("AWS_LAMBDA_LOG_STREAM_NAME").

%%
setup_erlcloud_cloudwatch_logs() ->
   meck:new(erlcloud_cloudwatch_logs, [unstick, passthrough]),
   meck:expect(erlcloud_cloudwatch_logs, 
      describe_log_streams, 
      fun(_, _, _) -> 
         {ok, [[{<<"uploadSequenceToken">>, <<"xxx">>}]], undefined} 
      end
   ),
   meck:expect(erlcloud_cloudwatch_logs,
      put_logs_events,
      fun(_, _, _, _, _) ->
         {ok, <<"xxx">>}
      end
   ).

unset_erlcloud_cloudwatch_logs() ->
   meck:unload(erlcloud_cloudwatch_logs).

%%
setup_io() ->
   meck:new(file, [unstick, passthrough]),
   meck:expect(file, 
      read_line, 
      fun(_) -> 
         {ok, <<"{\"h\":\"w\"}">>} 
      end
   ),
   meck:expect(file, 
      write, 
      fun(_, X) -> 
         case X of
            <<"{\"h\":\"w\"}">> ->
               ok;
            _ ->
               {error, badarg}
         end
      end
   ).

unset_io() ->
   meck:unload(file).

