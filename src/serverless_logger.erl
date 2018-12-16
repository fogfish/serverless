-module(serverless_logger).

-export([log/3]).

%%
%%
log(Type, Pid, Msg)
 when Type =:= emergency orelse Type =:= alert orelse Type =:= critical orelse Type =:= error ->
   io:format(message(Type, Pid, Msg));
log(Type, Pid, Msg)
 when Type =:= warning ->
   io:format(message(Type, Pid, Msg));
log(Type, Pid, Msg) 
 when Type =:= notice orelse Type =:= info orelse Type =:= debug ->
   io:format(message(Type, Pid, Msg)).

%%
%%
message(Type, Pid, [H | _] = Msg)
 when is_integer(H) ->
   erlang:iolist_to_binary(
      io_lib:format("[~p] ~p: ~s", [Type, Pid, Msg])
   );

message(Type, Pid, Msg)
 when is_map(Msg) ->
   erlang:iolist_to_binary(
      io_lib:format("[~p] ~p: ~s", [Type, Pid, jsx:encode(Msg)])
   );

message(Type, Pid, Msg) ->
   erlang:iolist_to_binary(
      io_lib:format("[~p] ~p: ~p", [Type, Pid, Msg])
   ).
