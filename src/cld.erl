-module(cld).

-export([start/0]).

start() ->
  sync:go(),

  BenchName = some_name,
  TestDir = "./test",

  ensure_started([sasl, crypto]),

  case application:load(cld) of
    ok -> ok;
    {error, {already_loaded, cld}} -> ok
  end,

  register(cld, self()),

  application:start(cld, permanent),

  MonRef = erlang:monitor(process, whereis(cld_sup)).

ensure_started(Applications) when is_list(Applications) ->
  [ensure_started(Application) || Application <- Applications];

ensure_started(Application) ->
  case application:start(Application) of
    ok ->
      ok;
    {error, {already_started, Application}} ->
      ok;
    Error ->
      throw(Error)
  end.
