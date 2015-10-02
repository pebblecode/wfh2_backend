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
  {[<<"GET">>, <<"PUT">>], Req, State}.

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
    _ ->
      WorkerId = erlang:binary_to_atom(WorkerIdBinding, utf8),
      Exists = wfh2_worker_sup:worker_exists(WorkerId),
      {Exists, Req2, State#rest_state{worker_id = WorkerId}}
  end.

put_json(Req, State) ->
  case State#rest_state.worker_id of
    undefined -> false;
    Id ->
      {ok, BodyRaw, Req2} = cowboy_req:body(Req),
      BodyDes = jsx:decode(BodyRaw, [return_maps, {labels, atom}]),
      case maps:get(location, BodyDes) of
        Location -> error_logger:info_msg("Location: ~p~n", [Location]),
                           case Location of
                             <<"InOffice">> -> wfh2_worker:set_wfo(Id);
                             <<"OutOfOffice">> -> wfh2_worker:set_wfh(Id)
                           end,
                           {true, Req2, State};

        {badkey, _}     -> {false, Req2, State}
      end
  end.

