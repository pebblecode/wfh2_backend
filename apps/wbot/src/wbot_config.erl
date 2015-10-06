-module(wbot_config).

-export([get_env/1]).

get_env(Key) ->
  {ok, Val} = application:get_env(wbot, Key),
  Val.
