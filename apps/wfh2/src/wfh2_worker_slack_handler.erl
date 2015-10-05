-module(wfh2_worker_slack_handler).

-export([
         init/3
         , rest_init/2
         , allowed_methods/2
         , malformed_request/2
         , is_authorized/2
         , content_types_accepted/2
         , handle_form_post/2
        ]).

-record(state, {request_body, auth_token}).

-define(WFOO_COMMAND, <<"/hh">>).
-define(WFO_COMMAND, <<"/ho">>).

init(_Proto, Req, Opts) ->
  {upgrade, protocol, cowboy_rest, Req, Opts}.

rest_init(Req, [Authtoken]) ->
  {ok, Req, #state{auth_token = Authtoken}}.

handle_form_post(Req, State) ->
  case State#state.request_body of
    #{ <<"user_id">> := SlackId, <<"command">> := Command } ->
      try sprof_cache:get_email_for(SlackId) of
        {ok, WorkerId} ->
          error_logger:info_msg("Id for worker: ~p~n", [WorkerId]),
          case  Command of
            ?WFOO_COMMAND -> error_logger:info_msg("Setting working out of office~n"),
                             Req2 = cowboy_req:set_resp_body(
                                      <<"Ok, setting working out of office.">>, Req),
                             {true, Req2, State};
            ?WFO_COMMAND  -> error_logger:info_msg("Setting working from office~n"),
                             Req2 = cowboy_req:set_resp_body(
                                      <<"Ok, setting working from office.">>, Req),
                             {true, Req2, State}
          end
      catch
        error:_ -> {false, Req, State} %userid not found
      end;
  _ -> {false, Req, State}
  end.

allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

malformed_request(Req, State) ->
  {ok, BodyKeyValuesRaw, Req2} = cowboy_req:body_qs(Req),
  error_logger:info_msg("verifying request body: ~p~n", [BodyKeyValuesRaw]),
  BodyKeyValues = maps:from_list(BodyKeyValuesRaw),
  RequiredFieldsPresent =
  maps:is_key(<<"token">>, BodyKeyValues)
  andalso maps:is_key(<<"user_id">>, BodyKeyValues)
  andalso maps:is_key(<<"command">>, BodyKeyValues),
  case RequiredFieldsPresent of
    true -> CommandValid = case maps:get(<<"command">>, BodyKeyValues) of
                             ?WFO_COMMAND -> true;
                             ?WFOO_COMMAND -> true;
                             _ -> false
                           end,
            { not CommandValid, Req2, State#state{request_body = BodyKeyValues}};
    false -> {true, Req2, State}
  end.

is_authorized(Req, State) ->
  Token = maps:get(<<"token">>, State#state.request_body),
  error_logger:info_msg("Matching provided token: ~p~n", [Token]),
  case lists:member(Token, State#state.auth_token) of
    true -> {true, Req, State};
    false -> { false, Req, State }
  end.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"x-www-form-urlencoded">>, []},
     handle_form_post}], Req, State}.

