-module(sprof_client).

-export([load_profiles/0]).

%%------------------------------------------------------------%%
% API
%%------------------------------------------------------------%%

load_profiles() ->
  try
    Body = make_request(),
    LookupProfileById = read_body(Body),
    {ok, LookupProfileById}
  catch
    Error:Reason -> { Error, Reason }
  end.

%%------------------------------------------------------------%% 
% private functions
%%------------------------------------------------------------%% 

make_request() ->
  Token = sprof_config:get_env(slackbot_token),
  UsersListEndpoint = "https://slack.com/api/users.list",
  Request = UsersListEndpoint ++ "?token=" ++ Token ++ "&presence=0",
  HttpOptions = [{ssl, [{verify, verify_none}]}],
  Opts = [{body_format, binary}],
  {ok, {{_, 200, _}, _, Body}} =
  httpc:request( get, {Request, []}, HttpOptions, Opts),
  Body.

read_body(Body) ->
  AllMembers =
  maps:get(members, jsx:decode(Body, [return_maps, {labels, atom}])),
  ActiveMembers = lists:filter(fun should_manage/1, AllMembers),
  maps:from_list([{maps:get(id, X), X} || X <- ActiveMembers]).

should_manage(User) ->
  case User of
    #{   deleted := false
         , is_restricted := false
         , is_bot := false
     } -> true;
    _ -> false
  end.

