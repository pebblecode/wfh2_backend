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

-export([get_channels/0
         , send_im/2
         , get_im_channel_for/1
        ]).

-record(state, {msg_count :: integer()}).

-define(SLACKBOT_TOKEN, sprof_config:get_env(slackbot_token)).
-define(SLACKURI, "https://slack.com/api").
-define(CONTENT_TYPE, "application/x-www-form-urlencoded").
-define(REQ_OPTS, [{body_format, binary}]).

start_link() ->
  Endpoint = "rtm.start",
  RequestBody = "token=" ++ ?SLACKBOT_TOKEN ++ "&simple_latest&no_unreads",
  Body = make_request(Endpoint, RequestBody),
  WSUrl = binary_to_list(maps:get(url, jsx:decode(Body, [return_maps, {labels, atom}]))),
  {ok, Pid} = websocket_client:start_link(WSUrl, ?MODULE, []),
  erlang:register(?MODULE, Pid),
  Pid ! {load_channels},
  {ok, Pid}.

init([]) ->
    {reconnect, #{}}.

make_request(Endpoint, RequestBody) ->
  Url = string:join([?SLACKURI, Endpoint], "/"),
  Req = {Url, [], ?CONTENT_TYPE, RequestBody},
  {ok, {{_,200,_}, _, Body}} = httpc:request(post, Req, [], ?REQ_OPTS),
  Body.

get_channels() ->
  Endpoint = "channels.list",
  RequestBody = "token=" ++ ?SLACKBOT_TOKEN ++ "&exclude_archived=1",
  Body = make_request(Endpoint, RequestBody),
  #{ok := true, channels := Channels} = jsx:decode(Body, [return_maps, {labels, atom}]),
  Channels.

get_im_channel_for(Email) ->
  Endpoint = "im.list",
  RequestBody = "token=" ++ ?SLACKBOT_TOKEN,
  Body = make_request(Endpoint, RequestBody),
  #{ok := true, ims := Ims} = jsx:decode(Body, [return_maps, {labels, atom}]),
  LookupImChannelByUserId = maps:from_list(lists:map(fun (Ch) -> {maps:get(user, Ch),
                                                          maps:get(id, Ch)} end,
                                                    Ims)),
  #{ id := UserId } = maps:get(Email, sprof_cache:get_profiles_by_email()),
  maps:get(UserId, LookupImChannelByUserId).

send_im(Email, Message) ->
  wbot_connection ! {send_im, Email, Message}.

onconnect(_WSReq, _State) ->
    {ok, #state{msg_count = 0}}.

ondisconnect({remote, closed}, State) ->
    {reconnect, State}.

websocket_handle({pong, _}, _ConnState, State) ->
    {ok, State};
websocket_handle({ping, _}, _ConnState, State) ->
  {reply, pong, State};
websocket_handle({text, Msg}, _ConnState, State) ->
  io:fwrite("Chat Message: ~p~n", [Msg]),
  {ok, State}.

websocket_info({send_im, Email, Message}, _ConnState, State) when
    is_binary(Message) ->
  Channel = get_im_channel_for(Email),
  MsgId = State#state.msg_count + 1,
  Body = #{id => MsgId, type => <<"message">>, channel => Channel, text =>
           Message},
  Enc = jsx:encode(Body),
  error_logger:info_msg("Sending message: ~p~n", [Enc]),
  {reply, {text, Enc}, State#state{msg_count = MsgId}};
websocket_info(Msg, _ConnState, State) ->
  error_logger:info_msg("Message received: ~p~n", [Msg]),
  {ok, State}.

websocket_terminate(Reason, _ConnState, State) ->
    io:format("Websocket closed in state ~p wih reason ~p~n",
              [State, Reason]),
    erlang:unregister(?MODULE),
    ok.

