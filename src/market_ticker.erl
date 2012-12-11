-module(market_ticker).
-behaviour(gen_server).
-include("market_data.hrl").

-export([start_link/0, init/1]).
-export([handle_event/2, handle_info/2, handle_call/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3]).

%% GEN_SERVER CALLBACKS

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  erlang:send_after(100, self(), tick),
  {ok, undefined}.

handle_event(_Event, S) -> {ok, S}.

handle_info(tick, S) ->
  market_events:quotes(market_data:quotes()),
  erlang:send_after(2000, self(), tick),
  {noreply, S};

handle_info(Msg, S) -> lager:info("INFO: ~p", [Msg]), {noreply, S}.

handle_call(_Msg, S) -> {reply, ok, S}.

handle_call(_Msg, _, S) -> {reply, ok, S}.

handle_cast(_Msg, S) -> {noreply, S}.

terminate(Reason, _) ->
  lager:info("~p terminating due to ~p", [?MODULE, Reason]),
  ok.

code_change(_, _, S) -> {ok, S}.
