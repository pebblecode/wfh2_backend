-module(wfh2_worker_collection_handler).

-export([init/3
        , allowed_methods/2
        , content_types_provided/2
        , resource_exists/2
        , get_json/2
        , options/2
        ]).

init(_Proto, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
  case cowboy_req:method(Req) of
    {<<"GET">>, Req2} ->
      {[{{<<"application">>, <<"json">>, []}, get_json}], Req2, State};
    {<<"OPTIONS">>, Req2} ->
      {[{{<<"application">>, <<"json">>, []}, options}], Req2, State}
  end.

options(Req, State) ->
  Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req),
  Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req1),
  {ok, Req2, State}.

resource_exists(Req, State) ->
  {true, Req, State}.

get_profile_and_state(WorkerId, GetWorkerState, GetWorkerProfile) ->
  State = GetWorkerState(WorkerId),
  Profile = GetWorkerProfile(WorkerId),
  {State, Profile}.

get_worker_infos(WorkerIds, GetWorkerState, GetWorkerProfile) ->
  lists:map(fun (Wid) ->
                get_profile_and_state(
                  Wid, GetWorkerState, GetWorkerProfile) end, WorkerIds).

get_worker_state(WorkerId) ->
  {ok, State} = wfh2_worker:get_worker_state(WorkerId),
  State.

get_worker_profile(WorkerId) ->
  sprof_cache:get_profile_for(WorkerId).

get_json(Req, State) ->
  WorkerIds = wfh2_worker_sup:get_worker_ids(),
  WorkerInfos = get_worker_infos(WorkerIds, fun get_worker_state/1, fun get_worker_profile/1),

  WorkerPresentations =
  lists:foldl(fun (Ele, Acc) ->
                  EncEle = wfh2_serialisation:encode_status(Ele),
                  case Acc =:= <<>> of
                    true ->
                      EncEle;
                    _ ->
                      <<Acc/binary, $,, EncEle/binary>>
                  end
              end, <<>>, WorkerInfos),

  Body = <<"[", WorkerPresentations/binary, "]">>,
  {Body, Req, State}.

