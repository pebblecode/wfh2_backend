-record(worker_state, {
          id :: atom()
          , version = 0 :: integer()
          , working_from = office :: wfh2_worker:location()
          , default = office :: wfh2_worker:location()
          , last_updated = erlang:timestamp() :: erlang:timestamp()
         }).

-type worker_state() :: #worker_state{}.

