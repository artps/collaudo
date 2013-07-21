-module(cld_worker_sup).
-behaviour(supervisor).

-include("cld.hrl").

-export([start_link/0,
         start_child/1,
         stop_child/1,
         stop_children/0,
         children/0]).
-export([init/1]).

-define(CHILD(Id, Mod, Type), {Id, {Mod, start_link, [Id]},
                                     permanent, 5000, Type, [Mod]}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(WorkerId) ->
  cld_stats:start_client(),
  supervisor:start_child(?MODULE, ?CHILD(WorkerId, cld_worker, worker)).

stop_child(WorkerId) ->
  ok = supervisor:terminate_child(?MODULE, WorkerId),
  ok = supervisor:delete_child(?MODULE, WorkerId),
  cld_stats:stop_client(),
  ok.

stop_children() ->
  [stop_child(X) || X <- children()].

children() ->
  [Id || {Id, _Pid, worker, [cld_worker]} <- supervisor:which_children(?MODULE)].


init([]) ->
  {ok, {{one_for_one, 5, 10}, []}}.
