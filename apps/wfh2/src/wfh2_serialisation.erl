-module(wfh2_serialisation).

-include("../include/worker_status.hrl").

-export([encode_status/1]).

-spec(encode_status(WorkerStatus :: worker_status()) -> binary()).

encode_status(#worker_status{email = Email, working_from = WorkingFrom, name = Name}) ->
  {StatusType, StatusDetails} =
  case WorkingFrom of
    {_, office } -> {<<"InOffice">>, <<"">>};
    {_, {out_of_office, Info}} -> {<<"OutOfOffice">>, Info}
  end,
  IsDefault = case WorkingFrom of
                {default, _} -> true;
                _ -> false
              end,

  jsx:encode(#{
    <<"email">> => Email,
    <<"name">> => Name,
    <<"status">> => #{
        <<"statusType">> => StatusType,
        <<"statusDetails">> => StatusDetails,
        <<"isDefault">> => IsDefault
       }}).

