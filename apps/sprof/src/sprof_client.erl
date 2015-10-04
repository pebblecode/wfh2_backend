-module(sprof_client).

-export([load_profiles/0]).

load_profiles() ->
  Token = sprof_config:get_env(slackbot_token),
  UsersListEndpoint = "https://slack.com/api/users.list",
  Request = UsersListEndpoint ++ "?token=" ++ Token ++ "&presence=0",
  httpc:request(get, {Request, []}, [{ssl, {verify, verify_none}}], []).

