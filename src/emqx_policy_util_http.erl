-module(emqx_policy_util_http).

-include("emqx_policy.hrl").

-export([requestSync/3, env_http_request/0]).

env_http_request() ->
  Config = application:get_env(emqx_policy, api, undefined),
  Method = proplists:get_value(method, Config, post),
  Url = proplists:get_value(url, Config),
  ServerKey = proplists:get_value(server_key, Config),
  #http_request{url = Url, method = Method, server_key = ServerKey}.

%%--------------------------------------------------------------------
%% HTTP Request
%%--------------------------------------------------------------------

requestSync(get, Url, Params) ->
  Req = {Url ++ "?" ++ mochiweb_util:urlencode(Params), []},
  reply_response(httpc:request(get, Req, [{autoredirect, true}], []));

requestSync(post, Url, Params) ->
  Req = {Url, [], "application/x-www-form-urlencoded", mochiweb_util:urlencode(Params)},
  reply_response(httpc:request(post, Req, [{autoredirect, true}], [])).

reply_response({ok, {{_, Code, _}, _Headers, Body}}) ->
  {ok, Code, Body};
reply_response({ok, Code, Body}) ->
  {ok, Code, Body};
reply_response({error, Error}) ->
  {error, Error}.




