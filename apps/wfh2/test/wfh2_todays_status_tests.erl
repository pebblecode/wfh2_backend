-module(wfh2_todays_status_tests).
-include_lib("eunit/include/eunit.hrl").
-include("../include/worker_state.hrl").

% http://stackoverflow.com/a/12531512/672760
convert_to_timestamp(Date) ->
  DateSeconds = calendar:datetime_to_gregorian_seconds(Date) - 62167219200,
  {DateSeconds div 1000000, DateSeconds rem 1000000, 0}.

yesterday(Date) ->
  calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) -1).

same_time_test() ->
  Now = erlang:timestamp(),
  CurrentDateTime = calendar:now_to_datetime(Now),
  State = #worker_state{
             last_updated = Now,
             working_from = {out_of_office, <<"def home">>},
             default = office},
  {DefaultOrTodays, _} = wfh2_todays_status:working_from(State, CurrentDateTime),
  ?assertEqual(DefaultOrTodays, todays_update).

updated_at_19_asking_next_day_at_9_should_return_default_test() ->
  Now = erlang:timestamp(),
  {Today, _} = calendar:now_to_datetime(Now),
  Yesterday = yesterday(Today),
  YesterdayAt19 = {Yesterday, {19,0,0}},
  TodayAt9 = {Today, {9,0,0}},
  State = #worker_state{last_updated = convert_to_timestamp(YesterdayAt19),
                        working_from = {out_of_office, <<"">>},
                        default = office},
  {DefaultOrTodays, _} = wfh2_todays_status:working_from(State, TodayAt9),
  ?assertEqual(DefaultOrTodays, default).

updated_at_20_asking_twodays_later_should_return_default_test() ->
  Now = erlang:timestamp(),
  {Today, _} = calendar:now_to_datetime(Now),
  InTwoDays = calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Today) + 2),
  InTwoDaysAt10 = {InTwoDays, {10,0,0}},
  UpdatedAt20 = {Today, {20,0,0}},
  State = #worker_state{last_updated = convert_to_timestamp(UpdatedAt20)},
  {DefaultOrTodays, _} = wfh2_todays_status:working_from(State, InTwoDaysAt10),
  ?assertEqual(DefaultOrTodays, default).

