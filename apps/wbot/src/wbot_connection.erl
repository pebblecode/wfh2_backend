-module('wbot_connection').

-behaviour(websocket_client).

-export([
         start_link/0,
         init/1,
         onconnect/2,
         ondisconnect/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3
        ]).

start_link() ->
  Token = wbot_config:get_env(slackbot_token),
  SlackUri = "https://slack.com/api/rtm.start",
  RequestBody = "token=" ++ Token ++ "&simple_latest&no_unreads",
  ContentType =  "application/x-www-form-urlencoded",
  Opts = [{body_format, binary}],
  Headers = [],
  Req = {SlackUri, Headers, ContentType, RequestBody},
  error_logger:info_msg("making Request: ~p~nWith HttpOpts: ~p~nAnd Opts:~p~nTo slack api",[Req, "", Opts]),
  {ok, {{_, 200, _}, _, Body}} = httpc:request(post, Req, [], Opts),
  WSUrl = binary_to_list(maps:get(url, jsx:decode(Body, [return_maps, {labels, atom}]))),
  {ok, Pid} = websocket_client:start_link(WSUrl, ?MODULE, []),
  erlang:register(?MODULE, Pid),
  {ok, Pid}.

init([]) ->
    {reconnect, #{}}.

onconnect(_WSReq, State) ->
    %websocket_client:cast(self(), {text, <<"message 1">>}),
    {ok, State}.

ondisconnect({remote, closed}, State) ->
    {reconnect, State}.

websocket_handle({pong, _}, _ConnState, State) ->
    {ok, State};
websocket_handle({ping, _}, _ConnState, State) ->
  {reply, pong, State};
websocket_handle({text, _Msg}, _ConnState, State) ->
    {ok, State}.
websocket_info(start, _ConnState, State) ->
    {reply, {text, <<"erlang message received">>}, State}.

websocket_terminate(Reason, _ConnState, State) ->
    io:format("Websocket closed in state ~p wih reason ~p~n",
              [State, Reason]),
    erlang:unregister(?MODULE),
    ok.

