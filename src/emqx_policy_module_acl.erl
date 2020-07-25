-module(emqx_policy_module_acl).

-behaviour(emqttd_acl_mod).

%% include
-include("emqx_policy.hrl").
-include_lib("emqttd/include/emqttd.hrl").

-import(emqx_policy_util_format, [validate_boolean/1]).
-import(emqx_policy_util_http, [requestSync/3, env_http_request/0]).
-import(emqx_policy_util_binary, [trimBOM/1]).
-import(emqx_policy_util_logger, [errorLog/2, infoLog/2]).

%% Callbacks
-export([init/1, check_acl/2, reload_acl/1, description/0]).

init(Env) ->
  {ok, Env}.

check_acl({#mqtt_client{client_id = ClientId}, PubSub, Topic}, _Env) ->
  access(PubSub, ClientId, Topic).

reload_acl(_State) -> ok.

access(subscribe, ClientId, Topic) ->
  request_acl_hook(ClientId, Topic, subscribe_acl, env_http_request());
access(publish, ClientId, Topic) ->
  request_acl_hook(ClientId, Topic, publish_acl, env_http_request()).


%%--------------------------------------------------------------------
%% Request Hook
%%--------------------------------------------------------------------

request_acl_hook(ClientId, Topic, Action, #http_request{method = Method, url = Url, server_key = ServerKey}) ->
  Mod = acl,
  Params = [
    {server_key, ServerKey}
    , {module, Mod}
    , {action, Action}
    , {client_id, ClientId}
    , {topic, Topic}
  ],
  case requestSync(Method, Url, Params) of {ok, Code, Body} ->
    infoLog("~nrequest_acl_hook ~nCode: ~p,~nBody: ~p~n", [Code, Body]),
    Json = trimBOM(list_to_binary(Body)),
    IsJson = jsx:is_json(Json),
    if
      IsJson ->
        handle_request_result(Json);
      true ->
        errorLog("~Acl Request JSON Format Error : ~p~n", [Body]),
        deny
    end;
    {error, Error} ->
      errorLog("~naction: ~p~nError: ~p~n", [Action, Error]),
      deny
  end.

handle_request_result(Json) ->
  JSONBody = jsx:decode(Json),
  case lists:keyfind(<<"acl_allow">>, 1, JSONBody) of {_, IsAllow} ->
    IsAllowFlag = validate_boolean(IsAllow),
    if IsAllowFlag ->
      allow;
      true ->
        deny
    end;
    _ ->
      deny
  end.

description() -> "Emq Policy Server ACL module".