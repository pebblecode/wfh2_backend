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
  Email = atom_to_binary(WorkerId, utf8),
  Name = case maps:get(real_name, Profile, Email) of
           <<"">> -> Email;
           Other -> Other
         end,
  jsx:encode(#{
    <<"email">> => Email,
    <<"name">> => Name,
    <<"status">> => #{
        <<"statusType">> => StatusType,
        <<"statusDetails">> => StatusDetails
       }}).

