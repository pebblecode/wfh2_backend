-record(worker_state, {
          id :: atom()
          , version = 0 :: integer()
          , working_from = office :: home | office
          , default = office :: {out_of_office, binary()} | office
          , info = <<"">> :: binary()
          , last_updated = erlang:timestamp() :: erlang:timestamp()
         }).

-type worker_state() :: #worker_state{}.

