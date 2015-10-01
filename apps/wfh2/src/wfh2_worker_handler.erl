-module(wfh2_worker_handler).

-export([init/3
        , rest_init/2
        , content_types_accepted/2
        , content_types_provided/2
        , allowed_methods/2
        , resource_exists/2
        , get_json/2
        , put_json/2]).

-record(state, {worker_id :: atom()}).

init(_Proto, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _State) ->
  {ok, Req, #state{}}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, put_json}],
   Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, get_json}], Req, State}.

encode_status({_Rec, _Id, _Name, _V, Email, WorkingFrom, Info, _, _}) ->
  jsx:encode(#{
    <<"email">> => list_to_binary(Email),
    <<"status">> => #{
        <<"statusType">> => atom_to_binary(WorkingFrom, utf8),
        <<"statusDetails">> =>
        case is_list(Info) of
          true -> list_to_binary(Info);
          false -> <<"">> end
       }}).

get_json(Req, State) ->
  WorkerId = State#state.worker_id,
  case WorkerId of

    undefined ->
      WorkerIds = wfh2_worker_sup:get_worker_ids(),
      WorkerStates = lists:map(fun (Wid) -> 
                                   {ok, WorkerState} =
                                     wfh2_worker:get_worker_state(Wid),
                                   WorkerState
                               end, WorkerIds),
      WorkerPresentations =
        lists:foldl(fun (WS, Acc) ->
                      NewPres = encode_status(WS),
                      <<Acc/binary, NewPres/binary, ",">>
                    end, <<>>, WorkerStates),

      Body = <<"[", WorkerPresentations/binary, "]">>,
      {Body, Req, State};

    _ ->
      WorkerState = wfh2_worker:get_worker_state(WorkerId),
      error_logger:info_msg("WorkerState: ~p~n", [WorkerState]),
      {ok, {_, _, _Name, _, Email,
             WorkingFrom, Info, _, _}} = WorkerState,
      Body = jsx:encode(#{
                         <<"email">> => list_to_binary(Email),
                         <<"status">> => #{
                             <<"statusType">> => atom_to_binary(WorkingFrom, utf8),
                             <<"statusDetails">> =>
                                case is_list(Info) of
                                  true -> list_to_binary(Info);
                                  false -> <<"">> end
                              }}),
      error_logger:info_msg("JSON: ~p~n", [Body]),
      {Body, Req, State}
    end.

resource_exists(Req, State) ->
  {WorkerIdBinding, Req2} = cowboy_req:binding(worker_id, Req),
  error_logger:info_msg("Binding: ~p", [WorkerIdBinding]),
  case WorkerIdBinding of
    undefined -> {true, Req2, State};
    _ ->
      WorkerId = erlang:binary_to_atom(WorkerIdBinding, utf8),
      Exists = wfh2_worker_sup:worker_exists(WorkerId),
      {Exists, Req2, State#state{worker_id = WorkerId}}
  end.

put_json(Req, State) ->
  {{true, <<"/">>}, Req, State}.

