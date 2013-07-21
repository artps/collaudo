-module(cld_manager).
-behaviour(gen_server).

-include("cld.hrl").

-export([start_worker/0,
         stop_workers/0,
         start_link/0]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {workers_num = 0}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start_worker() ->
  gen_server:call(?MODULE, start_worker).

stop_workers() ->
  gen_server:call(?MODULE, stop_workers).

init([]) ->
    {ok, #state{ }}.

handle_call(start_worker, _From, State) ->
  WorkerId = State#state.workers_num + 1,
  cld_worker_sup:start_child(WorkerId),
  {reply, ok, State#state{ workers_num = WorkerId }};

handle_call(stop_workers, _From, _State) ->
  cld_worker_sup:stop_children(),
  {reply, ok, #state{}};

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



