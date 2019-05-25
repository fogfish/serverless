%%
%% Copyright (C) 2018 Dmitry Kolesnikov
%%
%% This file may be modified and distributed under the terms
%% of the MIT license.  See the LICENSE file for details.
%% https://github.com/fogfish/serverless
%%
-module(ec2).
-export([main/1]).
-compile({parse_transform, category}).


%%
%%
main(Opts) ->
   serverless:spawn(fun ec2/1, Opts).

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

