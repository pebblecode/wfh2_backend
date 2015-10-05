-type event_type() :: location_updated | default_updated.

-record(event, {
          event_type :: event_type()
          , worker_id :: atom()
          , timestamp :: erlang:timestamp()
          , payload :: term()
         }).

-type event() :: #event{}.

