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

-define(WFOO_COMMAND, <<"/wfh">>).
-define(WFO_COMMAND, <<"/wfo">>).

init(_Proto, Req, Opts) ->
  {upgrade, protocol, cowboy_rest, Req, Opts}.

rest_init(Req, [Authtoken]) ->
  {ok, Req, #state{auth_token = Authtoken}}.

get_body_keyvalues(Req) ->
  {ok, BodyKeyValuesRaw, Req2} = cowboy_req:body_qs(Req),
  error_logger:info_msg("verifying request body: ~p~n", [BodyKeyValuesRaw]),
  {Req2, maps:from_list(BodyKeyValuesRaw)}.

handle_form_post(Req, State) ->
  case State#state.request_body of
    #{ <<"user_id">> := SlackId, <<"command">> := Command } ->
      try sprof_cache:get_email_for(SlackId) of
        {ok, WorkerId} ->
          error_logger:info_msg("Id for worker: ~p~n", [WorkerId]),
          case  Command of
            ?WFOO_COMMAND -> error_logger:info_msg("Setting working out of office~n"),
                             Info = maps:get(<<"text">>, State#state.request_body, <<"">>),
                             wfh2_worker:set_wfh(binary_to_atom(WorkerId, utf8), Info),
                             Req2 = cowboy_req:set_resp_body(
                                      <<"OK, setting working out of office.">>, Req),
                             {true, Req2, State};
            ?WFO_COMMAND  -> error_logger:info_msg("Setting working from office~n"),
                             wfh2_worker:set_wfo(binary_to_atom(WorkerId, utf8)),
                             Req2 = cowboy_req:set_resp_body(
                                      <<"OK, setting working from office.">>, Req),
                             {true, Req2, State};
            _ -> {false, Req, State}
          end
      catch
        error:_ -> {false, Req, State} %userid not found
      end;
  _ -> {false, Req, State}
  end.

allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

is_authorized(Req, State) ->
  {Req1, BodyKeyValues} = get_body_keyvalues(Req),
  Token = maps:get(<<"token">>, BodyKeyValues),
  error_logger:info_msg("Matching provided token: ~p~n", [Token]),
  case lists:member(Token, State#state.auth_token) of
    true -> {true, Req1, State#state{request_body = BodyKeyValues}};
    false -> {false, Req, State}
  end.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"x-www-form-urlencoded">>, []},
     handle_form_post}], Req, State}.

