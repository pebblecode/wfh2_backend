-record(worker_state, {
          id :: atom()
          , name = '' :: string()
          , version = 0 :: integer()
          , email = '' :: string()
          , working_from = office :: home | office
          , info = '' :: string()
          , last_updated = erlang:timestamp() :: erlang:timestamp()
          , slack_id = '' :: string()}).

-type worker_state() :: #worker_state{}.

