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
      {ok, Req3, State2} = options(Req2, State),
      {[{{<<"application">>, <<"json">>, []}, get_json}], Req3, State2};
    {<<"OPTIONS">>, Req2} ->
      {[{{<<"application">>, <<"json">>, []}, options}], Req2, State}
  end.

options(Req, State) ->
  Req1 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, <<"GET, OPTIONS">>, Req),
  Req2 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, <<"*">>, Req1),
  {ok, Req2, State}.

resource_exists(Req, State) ->
  {true, Req, State}.

get_json(Req, State) ->
  Now = calendar:now_to_datetime(erlang:timestamp()),
  WorkerStatuses = wfh2_todays_status:get_worker_statuses(Now),
  WorkerPresentations =
  lists:foldl(fun (Ele, Acc) ->
                  EncEle = wfh2_serialisation:encode_status(Ele),
                  case Acc =:= <<>> of
                    true ->
                      EncEle;
                    _ ->
                      <<Acc/binary, $,, EncEle/binary>>
                  end
              end, <<>>, WorkerStatuses),

  Body = <<"[", WorkerPresentations/binary, "]">>,
  {Body, Req, State}.

