-module(wfh2_todays_status).

-include("../include/worker_status.hrl").
-include("../include/worker_state.hrl").

-export([get_worker_statuses/1
        , get_worker_status/2]).

-define(HOUR_OF_DAYCHANGE, 19).

normalised_date({{Year, Month, Day}, {Hour, _, _}}, CutOffHour)
  when Hour > CutOffHour ->
  {Year, Month, Day + 1};
normalised_date({{Year, Month, Day}, _}, _CutOffHour) ->
  {Year, Month,Day}.

working_from(WorkerState, CurrentDateTime) ->
  LastUpdated = calendar:now_to_datetime(WorkerState#worker_state.last_updated),
  DateUpdateAppliesTo = normalised_date(LastUpdated, ?HOUR_OF_DAYCHANGE),
  DateShouldGetUpdateFor = normalised_date(CurrentDateTime, ?HOUR_OF_DAYCHANGE),
  ShouldGetCurrentUpdate =
  calendar:date_to_gregorian_days(DateShouldGetUpdateFor) =:=
  calendar:date_to_gregorian_days(DateUpdateAppliesTo),
  if ShouldGetCurrentUpdate ->
       {todays_update, WorkerState#worker_state.working_from};
     true -> {default, WorkerState#worker_state.default}
  end.

get_worker_update(WorkerState, #{profile := Profile}, CurrentDateTime) ->
  Id = WorkerState#worker_state.id,
  WorkingFrom = working_from(WorkerState, CurrentDateTime),
  Name = case maps:get(real_name, Profile, <<"">>) of
           <<"">> -> Id;
           Other -> Other
         end,

  #worker_status {
     email = Id
     , name = Name
     , working_from = WorkingFrom}.

get_profile_and_state(WorkerId, GetWorkerState, GetWorkerProfile) ->
  State = GetWorkerState(WorkerId),
  Profile = GetWorkerProfile(WorkerId),
  {State, Profile}.

get_worker_state(WorkerId) ->
  {ok, State} = wfh2_worker:get_worker_state(WorkerId),
  State.

get_worker_profile(WorkerId) ->
  sprof_cache:get_profile_for(WorkerId).

get_worker_status(WorkerId, ForDateTime) ->
  {WorkerState, WorkerProfile} = get_profile_and_state(WorkerId, fun get_worker_state/1, fun get_worker_profile/1),
  get_worker_update(WorkerState, WorkerProfile, ForDateTime).

get_worker_statuses(ForDateTime) ->
  WorkerIds = wfh2_worker_sup:get_worker_ids(),
  lists:map(fun (WorkerId) -> get_worker_status(WorkerId, ForDateTime) end, WorkerIds).

