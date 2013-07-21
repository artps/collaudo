-module(cld_stats).
-behaviour(gen_server).

-export([start_link/0,
         start_client/0,
         stop_client/0,
         request/0,
         reset/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

-define(CLIENT_TABLE, clients).
-define(REQUESTS_TABLE, requests).
-define(REQUEST_TIME_TABLE, request_time).

start_client() ->
  folsom_metrics:notify({?CLIENT_TABLE, {inc, 1}}).

stop_client() ->
  folsom_metrics:notify({?CLIENT_TABLE, {dec, 1}}).

request() ->
  folsom_metrics:notify({?REQUESTS_TABLE, {inc, 1}}).

reset() ->
  folsom_metrics:delete_metric(?REQUEST_TIME_TABLE),
  folsom_metrics:new_histogram(?REQUEST_TIME_TABLE).


start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  folsom_metrics:new_counter(?CLIENT_TABLE),
  folsom_metrics:new_counter(?REQUESTS_TABLE),
  folsom_metrics:new_histogram(?REQUEST_TIME_TABLE),
  {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
