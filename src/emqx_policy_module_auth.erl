%%%--------------------------------------------------------------------------------
%% The MIT License (MIT)
%%
%% Copyright (c) 2017 WhoopsChat
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/ or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in all
%% copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%%--------------------------------------------------------------------------------

-module(emqx_policy_module_auth).

-behaviour(emqttd_auth_mod).

%% include
-include("emqx_policy.hrl").
-include_lib("emqttd/include/emqttd.hrl").

-import(emqx_policy_util_format, [validate_boolean/1, replace_str/3]).
-import(emqx_policy_util_http, [requestSync/3, env_http_request/0]).
-import(emqx_policy_util_binary, [trimBOM/1]).
-import(emqx_policy_util_logger, [errorLog/2, infoLog/2]).

%% Callbacks
-export([init/1, check/3, description/0]).

init(Env) ->
  {ok, Env}.

check(#mqtt_client{username = Username, client_id = ClientId, client_pid = ClientPid}, Password, _Env) ->
  infoLog("~nauth log (user.auth):~nclient(~s/~s)~n", [Username, ClientId]),
  request_auth_hook(ClientPid, ClientId, Username, Password, user_auth, env_http_request()).

%%--------------------------------------------------------------------
%% Request Hook
%%--------------------------------------------------------------------

request_auth_hook(ClientPid, ClientId, Username, Password, Action, #http_request{method = Method, url = Url, server_key = ServerKey}) ->
  Mod = auth,
  Params = [
    {server_key, ServerKey}
    , {module, Mod}
    , {action, Action}
    , {client_id, ClientId}
    , {username, Username}
    , {password, Password}
  ],
  case requestSync(Method, Url, Params) of {ok, Code, Body} ->
    infoLog("~nrequest_auth_hook ~nCode: ~p,~nBody: ~p~n", [Code, Body]),
    Json = trimBOM(list_to_binary(Body)),
    IsJson = jsx:is_json(Json),
    if
      IsJson ->
        handle_request_result(ClientPid, ClientId, Username, Json);
      true ->
        {error, "Auth Request JSON Format Error"}
    end;
    {error, Error} ->
      errorLog("~naction: ~p~nError: ~p~n", [Action, Error]),
      {error, Error}
  end.

handle_request_result(_ClientPid, _ClientId, _Username, Json) ->
  JSONBody = jsx:decode(Json),
  case lists:keyfind(<<"is_user">>, 1, JSONBody) of {_, IsUser} ->
    IsUserFlag = validate_boolean(IsUser),
    if IsUserFlag ->
      case lists:keyfind(<<"is_super">>, 1, JSONBody) of {_, IsSuper} ->
        IsSuperFlag = validate_boolean(IsSuper),
        {ok, IsSuperFlag};
        _ ->
          {ok, false}
      end;
      true ->
        {error, "Auth Failure"}
    end;
    _ ->
      {error, "Auth Failure"}
  end.

description() -> "Emq Policy Server AUTH module".