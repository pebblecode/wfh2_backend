-module(sprof_client).

-export([load_profiles/0]).

load_profiles() ->
  try
    Token = sprof_config:get_env(slackbot_token),
    UsersListEndpoint = "https://slack.com/api/users.list",
    Request = UsersListEndpoint ++ "?token=" ++ Token ++ "&presence=0",
    {ok, {{_, 200, _}, _, Body}} = httpc:request(get, {Request, []}, [{ssl, [{verify, verify_none}]}], [{body_format, binary}]),
    AllMembers = maps:get(members, jsx:decode(Body, [return_maps, {labels, atom}])),
    ActiveMembers = lists:filter(fun should_manage/1, AllMembers),
    LookupProfileById = maps:from_list([{maps:get(id, X), X} || X <- ActiveMembers]),
    {ok, LookupProfileById}
  catch
    Error:Reason -> { Error, Reason }
  end.

should_manage(User) ->
  case User of
    #{   deleted := false
       , is_restricted := false
       , is_bot := false
      } -> true;
    _ -> false
  end.

