-module(ec2).
-export([main/1]).


%%
%%
main(_) ->
   application:ensure_all_started(erlcloud),
   serverless:spawn(fun ec2/1).

%%
%%
ec2(_) ->
   serverless:notice(#{spawn => ec2}),

   {ok, Config} = erlcloud_aws:auto_config(),
   erlcloud_ec2:describe_instances(Config).
