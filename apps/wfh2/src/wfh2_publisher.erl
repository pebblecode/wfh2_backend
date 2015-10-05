-module(wfh2_publisher).

-export([start_link/0
        , add_handler/2
        , delete_handler/2
        , location_updated/1]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_event:start_link({local, ?SERVER}).

add_handler(Handler, Args) ->
  gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
  gen_event:delete_handler(?SERVER, Handler, Args).

location_updated(Event) ->
  gen_event:notify(?SERVER, Event).

