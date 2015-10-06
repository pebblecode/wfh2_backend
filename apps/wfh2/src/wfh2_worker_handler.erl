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

-type(location () :: in_office | out_of_office).

-record(rest_state, {worker_id :: atom(), action :: binary(), location ::
                     location(), info:: binary() | undefined}).

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

get_post_request_data(Req) ->
  {Action, Req2} = cowboy_req:binding(action, Req),
  {WorkerId, Req3} = cowboy_req:binding(worker_id, Req2),
  {ok, BodyRaw, Req4} = cowboy_req:body(Req3),
  Body = jsx:decode(BodyRaw, [return_maps, {labels, atom}]),
  {{Action, WorkerId}, Body, Req4}.

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
  try get_post_request_data(Req) of
    {{ActionBinding, WorkerIdBinding},Body , Req2} ->
      case {ActionBinding, #{location := Location} = Body} of
        {<<"location">>, _} ->
          WorkerId = binary_to_atom(WorkerIdBinding, utf8),
          case Location of
            <<"InOffice">> -> wfh2_worker:set_wfo(WorkerId),
                              {true, Req2, State};
            <<"OutOfOffice">> -> Info = maps:get(info, Body, <<"">>),
                                 wfh2_worker:set_wfh(WorkerId, Info),
                                 {true, Req2, State};
            _ -> Body = jsx:encode(#{<<"Error">> => <<"Unknown location">>}),
                 Req3 = cowboy_req:set_resp_body(Body, Req2),
                 {false, Req3, State}
          end;
        {<<"default">>, _} ->
          WorkerId = binary_to_atom(WorkerIdBinding, utf8),
          case Location of
            <<"InOffice">> ->
              wfh2_worker:set_default(WorkerId, office),
                              {true, Req2,State};
            <<"OutOfOffice">> -> Info = maps:get(info, Body, <<"">>),
                                 wfh2_worker:set_default(WorkerId,
                                                         {out_of_office, Info}),
                                 {true, Req, State};
            _ -> Body = jsx:encode(#{<<"Error">> => <<"Unknown location">>}),
                 Req3 = cowboy_req:set_resp_body(Body, Req2),
                 {false, Req3, State}
          end
      end
  catch
    _:Reason ->
      error_logger:info_msg("Malformed request: ~p~n~p~n", [Reason, erlang:get_stacktrace()]),
      {false, Req, State}
  end.

