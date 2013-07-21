-module(cld_websocket_handler).
-behaviour(cowboy_websocket_handler).

-include("cld.hrl").

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
  erlang:start_timer(1000, self(), [{connected, true}]),
  {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
  {ok, _Reply} = process_message(Msg),
  {reply, {text, mochijson2:encode([{success, true}])}, Req, State};
websocket_handle(_Data, Req, State) ->
  {ok, Req, State}.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
  erlang:start_timer(1000, self(), get_metrics()),
  {reply, {text, mochijson2:encode(Msg)}, Req, State};
websocket_info(_Info, Req, State) ->
  {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
  cld_manager:stop_workers(),
  cld_stats:reset(),
  ok.

get_metrics() ->
  [get_metric(X) || X <- folsom_metrics:get_metrics()].

get_metric(Name) ->
  Metrics = case folsom_metrics:get_metric_info(Name) of
    [{_, [{type, histogram}]}] ->
      folsom_metrics:get_histogram_statistics(Name);
    _ ->
      folsom_metrics:get_metric_value(Name)
  end,
  {Name, Metrics}.

process_message(Msg) when is_binary(Msg) ->
  {struct, [{Type, Action} | _ ]} = mochijson2:decode(Msg),
  process_message(binary_to_atom(Type), Action).

process_message(action, Action) ->
  process_action(Action);
process_message(_, _) ->
  {ok, undefined_message_type}.

process_action(Action) when is_binary(Action) ->
  process_action(binary_to_atom(Action));
process_action(add_worker) ->
  {ok, cld_manager:start_worker()};
process_action(_) ->
  {ok, undefined_action}.

binary_to_atom(Value) ->
  list_to_atom(binary_to_list(Value)).
