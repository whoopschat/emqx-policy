-module(emqx_policy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  {ok, Sup} = emqx_policy_module_super:start_link(),
  ok = emqttd_access_control:register_mod(auth, emqx_policy_module_auth, []),
  ok = emqttd_access_control:register_mod(acl, emqx_policy_module_acl, []),
  emqx_policy_module_hook:load(application:get_all_env()),
  {ok, Sup}.

stop(_State) ->
  ok = emqttd_access_control:unregister_mod(auth, emqx_policy_module_auth),
  ok = emqttd_access_control:unregister_mod(acl, emqx_policy_module_acl),
  emqx_policy_module_hook:unload().