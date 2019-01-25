%% @doc
%%   mock serverless environment
-module(serverless_mock).
-include_lib("common_test/include/ct.hrl").

-export([
   test/3
]).

%%
%%
test(Lambda, Mock, Expect) ->
   init(Mock, Expect),
   Lambda:main([]),
   free().

init(Mock, Expect) ->
   tcp_init(),
   tcp_with_packet(fun(Packet) -> runtime(Packet, Mock, Expect) end),
   lambda_init().

free() ->
   meck:unload(serverless),
   meck:unload(gen_tcp),
   meck:unload(inet).

%%
%%
tcp_init() ->
   meck:new(inet, [unstick, passthrough]),
   meck:new(gen_tcp, [unstick, passthrough]),
   meck:expect(gen_tcp, connect, fun connect/4),
   meck:expect(gen_tcp, close,  fun disconnect/1),
   meck:expect(inet, peername, fun peername/1),
   meck:expect(inet, sockname, fun sockname/1),
   meck:expect(inet, setopts,  fun setopts/2).

connect(Host, Port, _Opts, _Timeout) ->
   {ok, 
      #{
         peer => {Host, Port}, 
         sock => {{127,0,0,1}, 65536}
      }
   }.

disconnect(_) ->
   ok.

peername(#{peer := Peer}) ->
   {ok, Peer}.

sockname(#{sock := Sock}) ->
   {ok, Sock}.

setopts(_, _) ->
   ok.

%%
%%
tcp_with_packet(Fun) ->
   meck:expect(gen_tcp, send,
      fun(_Sock, Ingress) ->
         Self = self(),
         spawn(
            fun() ->
               timer:sleep(10),
               case Fun(Ingress) of
                  undefined -> 
                     ok;
                  Egress ->
                     [Self ! {tcp, undefined, X} || X <- Egress]
               end
            end
         ),
         ok
      end
   ).

runtime(<<"GET /2018-06-01/runtime/invocation/next HTTP/1.1\r\n">>, Mock, _Expect) ->
   Bin = jsx:encode(Mock),
   Len = size(Bin),
   [
      <<"HTTP/1.1 200 OK\r\n">>,
      <<"Connection: keep-alive\r\n">>,
      <<"Content-Type: application/json\r\n">>,
      <<"Lambda-Runtime-Aws-Request-Id: mock\r\n">>,
      <<"Content-Length: ", (typecast:s(Len))/binary, "\r\n">>,
      <<"\r\n">>,
      Bin
   ];

runtime(<<"POST /2018-06-01/runtime/invocation/mock/error HTTP/1.1\r\n">>, _, _) ->
   [
      <<"HTTP/1.1 202 OK\r\n">>,
      <<"Connection: keep-alive\r\n">>,
      <<"\r\n">>
   ];

runtime(<<"POST /2018-06-01/runtime/invocation/mock/response HTTP/1.1\r\n">>, _, _) ->
   undefined;

runtime(<<"Connection:", _/binary>>, _, _) ->
   undefined;

runtime(Http, _Mock, Expect) ->
   case jsx:decode(Http, [return_maps]) of
      Expect ->
         [
            <<"HTTP/1.1 202 OK\r\n">>,
            <<"Connection: keep-alive\r\n">>,
            <<"\r\n">>
         ];
      Json ->
         ct:pal("no match ~p ~p~n", [Expect, Json]),
         [
            <<"HTTP/1.1 500 OK\r\n">>,
            <<"Connection: keep-alive\r\n">>,
            <<"\r\n">>
         ]
   end.

%%
%%
lambda_init() ->
   meck:new(serverless, [unstick, passthrough]),
   meck:expect(serverless, spawn, fun run/1).


run(Lambda) ->
   {ok, _} = application:ensure_all_started(serverless, permanent),
   X = serverless_lambda:lifecycle("http://127.0.0.1:8888/2018-06-01", Lambda),
   try
      X(#{})
   catch _:Reason ->
      free(),
      exit(Reason)
   end.

