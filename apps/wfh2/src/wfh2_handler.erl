-module(wfh2_handler).

-export([init/3
        %, content_types_accepted/2
        , content_types_provided/2
        , get_json/2]).

init(_Proto, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

%content_types_accepted(_Req, _State) ->
  %[{{<<"application">>, <<"json">>, []}, put_json}].

content_types_provided(Req, State) ->
  {[{<<"application/json">>, get_json}], Req, State}.

get_json(Req, State) ->
  Body = <<"{\"rest\": \"Hello World\"}">>,
  {Body, Req, State}.

