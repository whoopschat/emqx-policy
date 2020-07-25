-module(emqx_policy_module_super).

-behaviour(supervisor).

-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {{one_for_one, 0, 1}, []}}.

