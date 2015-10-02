-module(wfh2_serialisation).

-include("../include/worker_state.hrl").

-export([encode_status/1]).

-spec(encode_status(WorkerState :: worker_state()) -> binary() ).

encode_status(WorkerState) ->
  #worker_state{email = Email, working_from = WorkingFrom, info = Info } = WorkerState,
  jsx:encode(#{
    <<"email">> => list_to_binary(Email),
    <<"status">> => #{
        <<"statusType">> => atom_to_binary(WorkingFrom, utf8),
        <<"statusDetails">> =>
        case is_list(Info) of
          true -> list_to_binary(Info);
          false -> <<"">> end
       }}).

