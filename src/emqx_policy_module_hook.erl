-module(emqx_policy_module_hook).

%% include
-include("emqx_policy.hrl").
-include_lib("emqttd/include/emqttd.hrl").

-import(emqx_policy_util_format, [replace_str/3, format_from/1]).
-import(emqx_policy_util_http, [requestSync/3, env_http_request/0]).
-import(emqx_policy_util_logger, [errorLog/2, infoLog/2]).
-import(emqx_policy_util_binary, [trimBOM/1]).

-export([load/1, unload/0]).

%% hooks
-export([hook_client_subscribe/4, hook_client_unsubscribe/4]).
-export([hook_client_connected/3, hook_client_disconnected/3]).
-export([hook_session_subscribed/4, hook_session_unsubscribed/4]).
-export([hook_message_publish/2, hook_message_delivered/4, hook_message_ack/4]).

load(Env) ->
  emqttd:hook('client.subscribe', fun ?MODULE:hook_client_subscribe/4, [Env]),
  emqttd:hook('client.unsubscribe', fun ?MODULE:hook_client_unsubscribe/4, [Env]),
  emqttd:hook('session.subscribed', fun ?MODULE:hook_session_subscribed/4, [Env]),
  emqttd:hook('session.unsubscribed', fun ?MODULE:hook_session_unsubscribed/4, [Env]),
  emqttd:hook('client.connected', fun ?MODULE:hook_client_connected/3, [Env]),
  emqttd:hook('client.disconnected', fun ?MODULE:hook_client_disconnected/3, [Env]),
  emqttd:hook('message.publish', fun ?MODULE:hook_message_publish/2, [Env]),
  emqttd:hook('message.delivered', fun ?MODULE:hook_message_delivered/4, [Env]),
  emqttd:hook('message.acked', fun ?MODULE:hook_message_ack/4, [Env]).

unload() ->
  emqttd:unhook('client.subscribe', fun ?MODULE:hook_client_subscribe/4),
  emqttd:unhook('client.unsubscribe', fun ?MODULE:hook_client_unsubscribe/4),
  emqttd:unhook('session.subscribed', fun ?MODULE:hook_session_subscribed/4),
  emqttd:unhook('session.unsubscribed', fun ?MODULE:hook_session_unsubscribed/4),
  emqttd:unhook('client.connected', fun ?MODULE:hook_client_connected/3),
  emqttd:unhook('client.disconnected', fun ?MODULE:hook_client_disconnected/3),
  emqttd:unhook('message.publish', fun ?MODULE:hook_message_publish/2),
  emqttd:unhook('message.delivered', fun ?MODULE:hook_message_delivered/4),
  emqttd:unhook('message.acked', fun ?MODULE:hook_message_ack/4).

%%--------------------------------------------------------------------
%% Client Hook
%%--------------------------------------------------------------------

hook_client_subscribe(ClientId, Username, TopicTable, _Env) ->
  infoLog("~nclient log (client.subscribe):~nclient(~s/~s) will subscribe: ~p~n", [Username, ClientId, TopicTable]),
  request_client_subscribe_hook(ClientId, Username, TopicTable, client_subscribe, env_http_request()),
  {ok, TopicTable}.

hook_client_unsubscribe(ClientId, Username, TopicTable, _Env) ->
  infoLog("~nclient log (client.unsubscribe):~nclient(~s/~s) unsubscribe ~p~n", [ClientId, Username, TopicTable]),
  request_client_subscribe_hook(ClientId, Username, TopicTable, client_unsubscribe, env_http_request()),
  {ok, TopicTable}.

hook_session_subscribed(ClientId, Username, {Topic, Opts}, _Env) ->
  infoLog("~nsession(~s/~s) subscribed: ~p~n", [Username, ClientId, {Topic, Opts}]),
  request_session_subscribe_hook(ClientId, Username, Topic, session_subscribed, env_http_request()),
  {ok, {Topic, Opts}}.

hook_session_unsubscribed(ClientId, Username, {Topic, Opts}, _Env) ->
  infoLog("~nsession(~s/~s) unsubscribed: ~p~n", [Username, ClientId, {Topic, Opts}]),
  request_session_subscribe_hook(ClientId, Username, Topic, session_unsubscribed, env_http_request()),
  ok.

%% hook client connected
hook_client_connected(ConnAck, Client = #mqtt_client{client_id = ClientId}, _Env) ->
  infoLog("~nclient log (client.connected):~nclient ~s connected, connack: ~w~n", [ClientId, ConnAck]),
  request_client_connect_hook(Client, client_connected, env_http_request()),
  {ok, Client}.

%% hook client connected
hook_client_disconnected(Reason, Client = #mqtt_client{client_id = ClientId}, _Env) ->
  infoLog("~nclient log (client.disconnected):~nclient ~s disconnected, reason: ~w~n", [ClientId, Reason]),
  request_client_connect_hook(Client, client_disconnected, env_http_request()),
  ok.

%%--------------------------------------------------------------------
%% Message Hook
%%--------------------------------------------------------------------

%% transform message and return
hook_message_publish(Message = #mqtt_message{topic = <<"$SYS/", _/binary>>}, _Env) ->
  {ok, Message};

hook_message_publish(Message = #mqtt_message{topic = Topic, payload = Payload}, _Env) ->
  infoLog("~nmessage log (message.publish):~npublish ~s~n", [emqttd_message:format(Message)]),
  {FromClientId, FromUsername} = format_from(Message#mqtt_message.from),
  request_message_hook(FromClientId, FromUsername, Topic, Payload, message_publish, env_http_request()),
  {ok, Message}.

%% hook message delivered
hook_message_delivered(ClientId, Username, Message = #mqtt_message{topic = Topic, payload = Payload}, _Env) ->
  infoLog("~nmessage log (message.delivered):~ndelivered to client(~s/~s): ~s~n", [Username, ClientId, emqttd_message:format(Message)]),
  request_message_hook(ClientId, Username, Topic, Payload, message_delivered, env_http_request()),
  {ok, Message}.

%% hook message ask
hook_message_ack(ClientId, Username, Message = #mqtt_message{topic = Topic, payload = Payload}, _Env) ->
  infoLog("~nmessage log (message.acked):~nclient(~s/~s) acked: ~s~n", [Username, ClientId, emqttd_message:format(Message)]),
  request_message_hook(ClientId, Username, Topic, Payload, message_ask, env_http_request()),
  {ok, Message}.

%%--------------------------------------------------------------------
%% Request Hook
%%--------------------------------------------------------------------

request_session_subscribe_hook(ClientId, Username, Topic, Action, #http_request{method = Method, url = Url, server_key = ServerKey}) ->
  Mod = session,
  Params = [
    {server_key, ServerKey}
    , {module, Mod}
    , {action, Action}
    , {client_id, ClientId}
    , {username, Username}
    , {topic, Topic}
  ],
  case requestSync(Method, Url, Params) of {ok, Code, Body} ->
    infoLog("~naction: ~p~nCode: ~p~nBody: ~p~n", [Action, Code, Body]),
    Json = trimBOM(list_to_binary(Body)),
    IsJson = jsx:is_json(Json),
    if
      IsJson ->
        handle_request_result(ClientId, Json);
      true ->
        error
    end;
    {error, Error} ->
      errorLog("~naction: ~p~nError: ~p~n", [Action, Error]),
      error
  end.

request_client_subscribe_hook(ClientId, Username, TopicTable, Action, #http_request{method = Method, url = Url, server_key = ServerKey}) ->
  Mod = client,
  Params = [
    {server_key, ServerKey}
    , {module, Mod}
    , {action, Action}
    , {client_id, ClientId}
    , {username, Username}
    , {topics, jsx:encode(TopicTable)}
  ],
  case requestSync(Method, Url, Params) of {ok, Code, Body} ->
    infoLog("~naction: ~p~nCode: ~p~nBody: ~p~n", [Action, Code, Body]),
    Json = trimBOM(list_to_binary(Body)),
    IsJson = jsx:is_json(Json),
    if
      IsJson ->
        handle_request_result(ClientId, Json);
      true ->
        error
    end;
    {error, Error} ->
      errorLog("~naction: ~p~nError: ~p~n", [Action, Error]),
      error
  end.

request_client_connect_hook(#mqtt_client{client_id = ClientId, username = Username}, Action, #http_request{method = Method, url = Url, server_key = ServerKey}) ->
  Mod = client,
  Params = [
    {server_key, ServerKey}
    , {module, Mod}
    , {action, Action}
    , {client_id, ClientId}
    , {username, Username}
  ],
  case requestSync(Method, Url, Params) of {ok, Code, Body} ->
    infoLog("~naction: ~p~nCode: ~p~nBody: ~p~n", [Action, Code, Body]),
    Json = trimBOM(list_to_binary(Body)),
    IsJson = jsx:is_json(Json),
    if
      IsJson ->
        handle_request_result(ClientId, Json);
      true ->
        error
    end;
    {error, Error} ->
      errorLog("~naction: ~p~nError: ~p~n", [Action, Error]),
      error
  end.

request_message_hook(ClientId, Username, Topic, Payload, Action, #http_request{method = Method, url = Url, server_key = ServerKey}) ->
  Mod = message,
  Params = [
    {server_key, ServerKey}
    , {module, Mod}
    , {action, Action}
    , {client_id, ClientId}
    , {username, Username}
    , {topic, Topic}
    , {payload, Payload}
  ],
  case requestSync(Method, Url, Params) of {ok, Code, Body} ->
    infoLog("~naction: ~p~nCode: ~p~nBody: ~p~n", [Action, Code, Body]),
    Json = trimBOM(list_to_binary(Body)),
    IsJson = jsx:is_json(Json),
    if
      IsJson ->
        handle_request_result(ClientId, Json);
      true ->
        error
    end;
    {error, Error} ->
      errorLog("~naction: ~p~nError: ~p~n", [Action, Error]),
      error
  end.

handle_request_result(ClientId, Json) ->
  JSONBody = jsx:decode(Json),
  case lists:keyfind(<<"sub_list">>, 1, JSONBody) of {_, SubList} ->
    handleResultSub(ClientId, SubList);
    _ ->
      true
  end,
  case lists:keyfind(<<"un_sub_list">>, 1, JSONBody) of {_, UnSubList} ->
    handleResultUnSub(ClientId, UnSubList);
    _ ->
      true
  end,
  case lists:keyfind(<<"pub_list">>, 1, JSONBody) of {_, PubList} ->
    handleResultPub(ClientId, PubList);
    _ ->
      true
  end,
  ok.

handleResultPub(ClientId, PubList) when is_list(PubList) ->
  try
    lists:map(fun(Pub) ->
      if is_list(Pub) ->
        Len = erlang:length(Pub),
        if
          Len >= 2 ->
            Topic = lists:nth(1, Pub),
            Payload = lists:nth(2, Pub),
            Msg = emqttd_message:make(ClientId, 1, Topic, Payload),
            emqttd:publish(Msg),
            ok;
          true ->
            error
        end;
        true -> error end,
      Pub end, PubList)
  catch
    throw:Term ->
      Term;
    exit:Reason ->
      Reason;
    error:Reason ->
      Reason
  end,
  ok;
handleResultPub(_, _) ->
  ok.

handleResultSub(ClientId, SubList) when is_list(SubList) ->
  try
    Client = emqttd_cm:lookup(ClientId),
    ClientPid = Client#mqtt_client.client_pid,
    TopicTable = [{Topic, 1} || Topic <- SubList],
    ClientPid ! {subscribe, TopicTable}
  catch
    throw:Term ->
      Term;
    exit:Reason ->
      Reason;
    error:Reason ->
      Reason
  end,
  ok;
handleResultSub(_, _) ->
  ok.

handleResultUnSub(ClientId, UnSubList) when is_list(UnSubList) ->
  try
    Client = emqttd_cm:lookup(ClientId),
    ClientPid = Client#mqtt_client.client_pid,
    Topics = [Topic || Topic <- UnSubList],
    ClientPid ! {unsubscribe, Topics}
  catch
    throw:Term ->
      Term;
    exit:Reason ->
      Reason;
    error:Reason ->
      Reason
  end,
  ok;
handleResultUnSub(_, _) ->
  ok.