-module(wfh2_serialisation).

-include("../include/worker_state.hrl").

-export([encode_status/1]).

-spec(encode_status(WorkerState :: worker_state()) -> binary() ).

encode_status(WorkerState) ->
  #worker_state{id = WorkerId, working_from = WorkingFrom, info = Info } = WorkerState,
  jsx:encode(#{
    <<"email">> => atom_to_binary(WorkerId, utf8),
    <<"status">> => #{
        <<"statusType">> => atom_to_binary(WorkingFrom, utf8),
        <<"statusDetails">> => Info
       }}).

