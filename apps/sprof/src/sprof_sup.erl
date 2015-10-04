%%%-------------------------------------------------------------------
%% @doc sprof top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module('sprof_sup').

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
  PollInterval = sprof_config:get_env(poll_interval),
    {ok, {{one_for_all, 1, 5},
           [{sprof_cache, {sprof_cache, start_link, [PollInterval]},
            permanent, 5000, worker, [sprof_cache, sprof_client]}]} }.

%%====================================================================
%% Internal functions
%%====================================================================
