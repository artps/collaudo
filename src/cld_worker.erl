-module(cld_worker).
-behaviour(gen_fsm).

-include("cld.hrl").

-export([start_link/1]).
-export([init/1,
         state_idle/2,
         state_idle/3,
         state_working/2,
         state_working/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-define(SERVER, ?MODULE).
-define(IDLE_TIMEOUT, 5000).

-record(state, { worker_id, timestamp }).

start_link(WorkerId) ->
  gen_fsm:start_link(?MODULE, [WorkerId], []).


init([WorkerId]) ->
  State = #state{
    worker_id = WorkerId
  },
  {ok, state_idle, State, ?IDLE_TIMEOUT}.


state_idle(timeout, State) ->
  gen_fsm:send_event(self(), run),
  {next_state, state_working, State};

state_idle(_Event, _State) ->
  {next_state, state_idle, ?IDLE_TIMEOUT}.

state_idle(_Event, _From, State) ->
  {reply, ok, state_idle, State}.


state_working(run, State) ->
  do(),
  {next_state, state_idle, State, ?IDLE_TIMEOUT div 2}.

state_working(_Event, _From, State) ->
  {reply, ok, state_idle, State, ?IDLE_TIMEOUT}.


handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  {reply, ok, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.



do() ->
  Start = os:timestamp(),
  timer:sleep(random:uniform(10000)),
  Elapsed = time_diff(Start),
  cld_stats:request(),
  folsom_metrics:notify({request_time, Elapsed}),
  ok.


time_diff(Start)->
  time_diff(Start, os:timestamp()).

time_diff(Start, End) ->
  erlang:max(0, round(timer:now_diff(End, Start) / 1000)).
