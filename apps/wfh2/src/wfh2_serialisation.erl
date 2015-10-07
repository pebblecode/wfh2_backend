-module(wfh2_serialisation).

-include("../include/worker_state.hrl").

-export([encode_status/1]).

-spec(encode_status(WorkerState :: worker_state()) -> binary() ).

encode_status({WorkerState, #{profile := Profile}}) ->
  #worker_state{id = WorkerId, working_from = WorkingFrom } = WorkerState,
  {StatusType, StatusDetails} = case WorkingFrom of
                                  {out_of_office, Info} ->
                                    {<<"OutOfOffice">>,Info};
                                  _ -> {<<"InOffice">>, <<"">> }
                                end,
  Name = maps:get(real_name, Profile, atom_to_binary(WorkerId, utf8)),
  jsx:encode(#{
    <<"email">> => atom_to_binary(WorkerId, utf8),
    <<"name">> => Name,
    <<"status">> => #{
        <<"statusType">> => StatusType,
        <<"statusDetails">> => StatusDetails
       }}).

