-module(ec2_SUITE).

-export([all/0]).
-export([ec2/1]).

all() ->
   [Test || {Test, NAry} <- ?MODULE:module_info(exports), 
      Test =/= module_info,
      Test =/= init_per_suite,
      Test =/= end_per_suite,
      NAry =:= 1
   ].

ec2(_) ->
   meck:new(erlcloud_aws, [unstick, passthrough]),
   meck:new(erlcloud_ec2, [unstick, passthrough]),

   meck:expect(erlcloud_aws, auto_config, fun() -> {ok, undefined} end),
   meck:expect(erlcloud_ec2, describe_instances, fun(_) -> {ok, instances()} end),

   serverless:mock(ec2, #{}, [[<<"1">>, <<"2">>], [<<"3">>, <<"4">>]]),

   meck:unload(erlcloud_ec2),
   meck:unload(erlcloud_aws).

instances() ->
   [
      [
         {instances_set, [
            [{instance_id, 1}],
            [{instance_id, 2}]
         ]}
      ],
      [
         {instances_set, [
            [{instance_id, 3}],
            [{instance_id, 4}]
         ]}
      ]
   ].