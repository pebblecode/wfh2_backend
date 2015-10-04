-module(sprof_config).

-export([get_env/1]).

get_env(Key) ->
  {ok,Value} = application:get_env(Key),
  Value.
