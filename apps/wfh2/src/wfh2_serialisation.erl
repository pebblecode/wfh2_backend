-module(wfh2_serialisation).

-include("../include/worker_state.hrl").

-export([encode_status/2]).

-spec(encode_status(WorkerId :: atom(), WorkerState :: worker_state()) -> binary() ).

encode_status(WorkerId, WorkerState) ->
  #worker_state{working_from = WorkingFrom, info = Info } = WorkerState,
  jsx:encode(#{
    <<"email">> => atom_to_binary(WorkerId, utf8),
    <<"status">> => #{
        <<"statusType">> => atom_to_binary(WorkingFrom, utf8),
        <<"statusDetails">> =>
        case is_list(Info) of
          true -> list_to_binary(Info);
          false -> <<"">> end
       }}).

