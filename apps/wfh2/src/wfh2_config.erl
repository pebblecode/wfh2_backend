-module(wfh2_config).

-export([get_env/1]).

get_env(Name) ->
  {ok, Var} =application:get_env(Name),
  Var.

