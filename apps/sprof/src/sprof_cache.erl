-module(sprof_cache).

-behaviour(gen_server).

%% API functions
-export([start_link/1
        , get_profiles/0
        , get_profiles_by_email/0
        , get_email_for/1
        , get_profile_for/1
        , get_emails/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {lookup_profile_by_slack_id = #{}, lookup_profile_by_email = #{}, poll_interval = 60000}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(PollInterval) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [PollInterval], []).

get_profiles() ->
  gen_server:call(?MODULE, get_profiles).

get_profiles_by_email() ->
  {ok, Profiles} = gen_server:call(?MODULE, get_profiles_by_email),
  Profiles.

get_email_for(Id) ->
  {ok, Profiles} = get_profiles(),
  Email = get_email_for(Id, Profiles),
  {ok, Email}.

get_email_for(Id, Profiles) ->
  #{ profile := #{email := Email } } = maps:get(Id, Profiles),
  Email.

get_profile_for(Email) when is_atom(Email) ->
  Profiles = ?MODULE:get_profiles_by_email(),
  maps:get(atom_to_binary(Email, utf8), Profiles);
get_profile_for(Email) when is_binary(Email) ->
  Profiles = ?MODULE:get_profiles_by_email(),
  maps:get(Email, Profiles).
  

get_emails() ->
  {ok, Profiles} = get_profiles(),
  lists:map(fun ({_K, Px}) -> #{ profile := #{ email := Email}} = Px, binary_to_list(Email) end, maps:to_list(Profiles)).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([PollInterval]) ->
  erlang:send_after(1, self(), trigger),
  {ok, #state{poll_interval = PollInterval}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_profiles, _From, State) ->
  {reply, {ok, State#state.lookup_profile_by_slack_id}, State};

handle_call(get_profiles_by_email, _From, State) ->
  {reply, {ok, State#state.lookup_profile_by_email}, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(trigger, State) ->
  error_logger:info_msg("triggered, loading profiles~n"),
  {ok, Profiles} = sprof_client:load_profiles(),
  LookupProfileBySlackId =
    maps:from_list([{maps:get(id, X), X} || X <- Profiles]),

  GetEmail = fun(X) ->
                 #{profile := #{ email := Email }} = X,
                 Email
             end,

  LookupProfileByEmail =
    maps:from_list([{GetEmail(X), X} || X <- Profiles]),
  error_logger:info_msg("profiles refreshed~n"),
  erlang:send_after(State#state.poll_interval, self(), trigger),
  {noreply, State#state{lookup_profile_by_slack_id = LookupProfileBySlackId
                       , lookup_profile_by_email = LookupProfileByEmail}};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
