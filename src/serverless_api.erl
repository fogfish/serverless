%% @doc
%%   AWS API Gateway integrations
-module(serverless_api).

-export([
   return/1
]).

%%
%%
return({error, Reason}) ->
   {Code, _} = Error = error_code(Reason),
   Json = error_json(Error, Reason),
   serverless:error(#{
      api    => error,
      reason => Json,
      status => Code
   }),
   return({Code, Json});

return({Code, Json}) when is_map(Json) orelse is_list(Json) ->
   return({Code, jsx:encode(Json)});

return({Code, Text}) when is_binary(Text) ->
   {ok, 
      #{
         statusCode => return_code(Code),
         body       => Text,
         headers    => default_cors()
      }
   };

return({Code, Head, Json}) when is_map(Json) orelse is_list(Json) ->
   return({Code, Head, jsx:encode(Json)});

return({Code, Head, Text}) when is_binary(Text) ->
   {ok, 
      #{
         statusCode => return_code(Code),
         body       => Text,
         headers    => maps:merge(Head, default_cors())
      }
   }.
   
return_code({Code, _}) ->
   Code;
return_code(Code) when is_tuple(Code) ->
   erlang:element(1,  status_code(erlang:element(1, Code)));
return_code(Code) ->
   erlang:element(1,  status_code(Code)).

%%
%%
error_code({Reason, _}) ->
   status_code(Reason);
error_code({require, _, ActualCode})
 when is_integer(ActualCode) ->
   status_code(ActualCode);
error_code(Code) when is_tuple(Code) ->
   status_code(erlang:element(1, Code));
error_code(Reason) ->
   status_code(Reason).

error_json({Code, Text}, Reason) ->
   #{
      type    => filename:join([<<"https://httpstatuses.com">>, typecast:s(Code)]),
      status  => Code,
      title   => Text,
      details => error_reason(Reason)
   }.

error_reason({_, #{} = Reason}) ->
   Reason;
error_reason({Reason, Details}) ->
   erlang:iolist_to_binary(
      io_lib:format("~2048.p : ~2048.p", [Reason, Details])
   );
error_reason(#{} = Reason) ->
   Reason;
error_reason(Reason) ->
   erlang:iolist_to_binary(
      io_lib:format("~2048.p", [Reason])
   ).


%%
%%
default_cors() ->
   #{
      <<"Access-Control-Allow-Origin">> => <<"*">>,
      <<"Access-Control-Allow-Methods">> => <<"GET, PUT, POST, DELETE, OPTIONS">>,
      <<"Access-Control-Allow-Headers">> => <<"Content-Type, Authorization, Accept">>,
      <<"Access-Control-Max-Age">> => 600
   }.   

%%
%%
status_code(100) -> {100, <<"Continue">>};
status_code(101) -> {101, <<"Switching Protocols">>};
status_code(200) -> {200, <<"OK">>};
status_code(201) -> {201, <<"Created">>};
status_code(202) -> {202, <<"Accepted">>};
status_code(203) -> {203, <<"Non-Authoritative Information">>};
status_code(204) -> {204, <<"No Content">>};
status_code(205) -> {205, <<"Reset Content">>};
status_code(206) -> {206, <<"Partial Content">>};
status_code(300) -> {300, <<"Multiple Choices">>};
status_code(301) -> {301, <<"Moved Permanently">>};
status_code(302) -> {302, <<"Found">>};
status_code(303) -> {303, <<"See Other">>};
status_code(304) -> {304, <<"Not Modified">>};
status_code(307) -> {307, <<"Temporary Redirect">>};
status_code(400) -> {400, <<"Bad Request">>};
status_code(401) -> {401, <<"Unauthorized">>};
status_code(402) -> {402, <<"Payment Required">>};
status_code(403) -> {403, <<"Forbidden">>};
status_code(404) -> {404, <<"Not Found">>};
status_code(405) -> {405, <<"Method Not Allowed">>};
status_code(406) -> {406, <<"Not Acceptable">>};
status_code(407) -> {407, <<"Proxy Authentication Required">>};
status_code(408) -> {408, <<"Request Timeout">>};
status_code(409) -> {409, <<"Conflict">>};
status_code(410) -> {410, <<"Gone">>};
status_code(411) -> {411, <<"Length Required">>};
status_code(412) -> {412, <<"Precondition Failed">>};
status_code(413) -> {413, <<"Request Entity Too Large">>};
status_code(414) -> {414, <<"Request-URI Too Long">>};
status_code(415) -> {415, <<"Unsupported Media Type">>};
status_code(416) -> {416, <<"Requested Range Not Satisfiable">>};
status_code(417) -> {417, <<"Expectation Failed">>};
status_code(422) -> {422, <<"Unprocessable Entity">>};
status_code(500) -> {500, <<"Internal Server Error">>};
status_code(501) -> {501, <<"Not Implemented">>};
status_code(502) -> {502, <<"Bad Gateway">>};
status_code(503) -> {503, <<"Service Unavailable">>};
status_code(504) -> {504, <<"Gateway Timeout">>};
status_code(505) -> {505, <<"HTTP Version Not Supported">>};

%status_code(100) -> <<"100 Continue">>;
%status_code(101) -> <<"101 Switching Protocols">>;
status_code(ok)       -> status_code(200);
status_code(created)  -> status_code(201);
status_code(accepted) -> status_code(202);
%status(203) -> <<"203 Non-Authoritative Information">>;
status_code(no_content) -> status_code(204);
%status(205) -> <<"205 Reset Content">>;
%status(206) -> <<"206 Partial Content">>;
%status(300) -> <<"300 Multiple Choices">>;
%status(301) -> <<"301 Moved Permanently">>;
status_code(redirect) -> status_code(302);
%status(303) -> <<"303 See Other">>;
%status(304) -> <<"304 Not Modified">>;
%status(307) -> <<"307 Temporary Redirect">>;
status_code(badarg) -> status_code(400);
status_code(unauthorized) -> status_code(401);
status_code(expired) -> status_code(401);
%status(402) -> <<"402 Payment Required">>;
status_code(forbidden) -> status_code(403);
status_code(not_found) -> status_code(404);
status_code(enoent)    -> status_code(404);
status_code(not_allowed)    -> status_code(405);
status_code(not_acceptable) -> status_code(406);
%status(407) -> <<"407 Proxy Authentication Required">>;
%status(408) -> <<"408 Request Timeout">>;
status_code(conflict) -> status_code(409);
status_code(duplicate)-> status_code(409);
%status(410) -> <<"410 Gone">>;
%status(411) -> <<"411 Length Required">>;
%status(412) -> <<"412 Precondition Failed">>;
%status(413) -> <<"413 Request Entity Too Large">>;
%status(414) -> <<"414 Request-URI Too Long">>;
status_code(bad_mime_type) -> status_code(415);
%status(416) -> <<"416 Requested Range Not Satisfiable">>;
%status(417) -> <<"417 Expectation Failed">>;
%status(422) -> <<"422 Unprocessable Entity">>;
status_code(required) -> status_code(500);
status_code(not_implemented) -> status_code(501);
%status(502) -> <<"502 Bad Gateway">>;
status_code(not_available) -> status_code(503);
%status(504) -> <<"504 Gateway Timeout">>;
%status(505) -> <<"505 HTTP Version Not Supported">>.
status_code(Code) when is_atom(Code) -> status_code(500);
status_code(Code) when is_integer(Code) -> status_code(Code).
