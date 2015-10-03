-module(wfh2_worker_handler).

-include("../include/worker_state.hrl").

-export([init/3
        , rest_init/2
        , content_types_accepted/2
        , content_types_provided/2
        , allowed_methods/2
        , resource_exists/2
        , get_json/2
        , put_json/2]).

-record(rest_state, {worker_id :: atom()}).

init(_Proto, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _State) ->
  {ok, Req, #rest_state{}}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"PUT">>, <<"POST">>], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, put_json}],
   Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, get_json}], Req, State}.

get_json(Req, State) ->
  WorkerId = State#rest_state.worker_id,
  {ok, WorkerState} = wfh2_worker:get_worker_state(WorkerId),
  error_logger:info_msg("WorkerState: ~p~n", [WorkerState]),
  Body = wfh2_serialisation:encode_status(WorkerState),
  error_logger:info_msg("JSON: ~p~n", [Body]),
  {Body, Req, State}.

resource_exists(Req, State) ->
  {WorkerIdBinding, Req2} = cowboy_req:binding(worker_id, Req),
  error_logger:info_msg("Binding: ~p", [WorkerIdBinding]),

  case WorkerIdBinding of

    undefined -> {false, Req2, State};

    _         -> WorkerId = erlang:binary_to_atom(WorkerIdBinding, utf8),
                 Exists = wfh2_worker_sup:worker_exists(WorkerId),
                 {Exists, Req2, State#rest_state{worker_id = WorkerId}}
  end.

put_json(Req, State) ->

  case State#rest_state.worker_id of
    undefined -> false;
    Id ->
      {ActionBinding, Req2} = cowboy_req:binding(action, Req),

      case ActionBinding of

        <<"location">> ->
          {ok, BodyRaw, Req3} = cowboy_req:body(Req2),

          try jsx:decode(BodyRaw, [return_maps, {labels, atom}]) of
            BodyDes ->
              try maps:get(location, BodyDes) of
                Location -> error_logger:info_msg("Location: ~p~n", [Location]),
                            Body = case Location of
                                     <<"InOffice">>     -> wfh2_worker:set_wfo(Id),
                                                           jsx:encode(#{ok => <<"in the office">>});
                                     <<"OutOfOffice">>  -> Info = binary_to_list(maps:get(details, BodyDes, <<"">>)),
                                                           wfh2_worker:set_wfh(Id, Info),
                                                           jsx:encode(#{ok => <<"home">>})
                                   end,
                            Req4 = cowboy_req:set_resp_body(Body, Req3),
                            {true, Req4, State}
              catch
                error:{badkey, location } -> Body = jsx:encode(#{error => <<"location field missing">>}),
                                             Req4 = cowboy_req:set_resp_body(Body, Req3),
                                             {false, Req4, State}
              end
          catch
            error:badarg -> Body = jsx:encode(#{error => <<"request body could not be read">>}),
                            Req4 = cowboy_req:set_resp_body(Body, Req3),
                            {false, Req4, State}
          end;

        _ -> Body = jsx:encode(#{ error => <<"Unsupported action">> }),
             Req3 = cowboy_req:set_resp_body(Body, Req2),
             { false, Req3, State }
      end
  end.

