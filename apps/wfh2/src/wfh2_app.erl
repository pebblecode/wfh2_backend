%%%-------------------------------------------------------------------
%% @doc wfh2 public API
%% @end
%%%-------------------------------------------------------------------

-module('wfh2_app').

-behaviour(application).

%% Application callbacks
-export([start/2
        ,stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  Port = wfh2_config:get_env(cowboy_port),
  error_logger:info_msg("Starting cowboy web server on port: ~p~.n", [Port]),
  Dispatch = cowboy_router:compile(
               [{'_',
                 [{"/workers/",
                   wfh2_worker_collection_handler, []},
                  { "/workers/:worker_id/[:action]",
                    wfh2_worker_handler, [] }
                 ]}]),

  {ok, _} = cowboy:start_http(http, 100, [{port, Port}],
                              [{env, [{dispatch, Dispatch}]}]),
  wfh2_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================

