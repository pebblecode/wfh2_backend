-module(wfh2_handler).

-export([init/3
        , content_types_accepted/2
        , content_types_provided/2
        , allowed_methods/2
        , resource_exists/2
        , get_json/2
        , put_json/2]).

init(_Proto, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"PUT">>], Req, State}.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, put_json}],
   Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, get_json}], Req, State}.

get_json(Req, State) ->
  Body = <<"{\"rest\": \"Hello World\"}">>,
  {Body, Req, State}.

resource_exists(Req, State) ->
  {WorkerIdBinding, Req2} = cowboy_req:binding(worker_id, Req),
  error_logger:info_msg("Binding: ~p", [WorkerIdBinding]),
  case WorkerIdBinding of
    undefined -> {false, Req2, State};
    _ ->
      WorkerId = erlang:binary_to_atom(WorkerIdBinding, utf8),
      Workers = supervisor:which_children(wfh2_worker_sup),
      error_logger:info_msg("Workers: ~p~n", [Workers]),
      Exists = lists:keysearch(WorkerId, 1, Workers) =/= false,
      error_logger:info_msg("Worker exists is: ~p~n", [Exists]),
      {Exists, Req2, State}
  end.

put_json(Req, State) ->
  {{true, <<"/">>}, Req, State}.

