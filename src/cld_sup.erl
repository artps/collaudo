-module(cld_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(CHILD(Mod, Type), {Mod, {Mod, start_link, []},
                                     permanent, 5000, Type, [Mod]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {{one_for_one, 5, 10}, [
    ?CHILD(cld_stats, worker),
    ?CHILD(cld_manager, worker),
    ?CHILD(cld_worker_sup, supervisor)
  ]}}.
