-module(wfh2_worker_slack_handler).

-export([
         init/3
         , rest_init/2
         , allowed_methods/2
         , is_authorized/2
         , content_types_accepted/2
         , handle_form_post/2
        ]).

-record(state, {request_body, auth_token}).

-define(WFOO_COMMAND, <<"/hfw2">>).
-define(WFO_COMMAND, <<"/hfo2">>).

init(_Proto, Req, Opts) ->
  {upgrade, protocol, cowboy_rest, Req, Opts}.

rest_init(Req, [Authtoken]) ->
  {ok, Req, #state{auth_token = Authtoken}}.

handle_form_post(Req, State) ->
  try maps:get(<<"command">>, State) of
    ?WFOO_COMMAND ->
      SlackId = maps:get(<<"user_id">>, State),
      WorkerId = sprof_cache:get_email_for(SlackId),
      error_logger:info_msg("Id for worker: ~p~n", [WorkerId]),
      {true, Req, State};
    ?WFO_COMMAND  -> {true, Req, State}
  catch
    {badkey, _}  -> {false, Req, State}
  end.

allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

is_authorized(Req, State) ->
  {ok, BodyKeyValuesRaw, Req2} = cowboy_req:body_qs(Req),
  BodyKeyValues = maps:from_list(BodyKeyValuesRaw),
  error_logger:info_msg("Body posted: ~p~n", [BodyKeyValues]),
  try maps:get(<<"token">>, BodyKeyValues) of
    Token -> error_logger:info_msg(
               "Matching provided token: ~p~n", [Token]),
             case Token =:= State#state.auth_token of
               true -> {true, Req2, BodyKeyValues};
               false -> { false, Req2, State }
             end
   
  catch
    {badkey, _} ->
      {{false,<<"Payload field=\"token\"">>}, Req2, State}
  end.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"x-www-form-urlencoded">>, []},
     handle_form_post}], Req, State}.

