-module(ec2).
-export([main/1]).
-compile({parse_transform, category}).


%%
%%
main(_) ->
   serverless:spawn(fun ec2/1).

%%
%%
ec2(_) ->
   [either ||
      serverless:notice(#{spawn => ec2}),
      erlcloud_aws:auto_config(),
      erlcloud_ec2:describe_instances(_),
      cats:unit(lens:get(ids(), _))
   ].

ids() ->
   lens:c(
      lens:traverse(),
      lens:pair(instances_set),
      lens:traverse(),
      lens:pair(instance_id),
      fun(Fun, Focus) ->
         lens:fmap(fun(_) -> Focus end, Fun(typecast:s(Focus)))
      end
   ).

