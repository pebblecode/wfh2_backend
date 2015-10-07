-record(worker_status, {
          email :: binary()
          , name :: binary()
          , working_from
          :: {default, wfh2_worker:location()}
          | {todays_update, wfh2_worker:location()}
         }).

-type worker_status() :: #worker_status{}.

