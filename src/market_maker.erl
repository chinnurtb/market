-module(market_maker).
-behaviour(gen_server).
-include("market_data.hrl").

-export([start_link/0, init/1]).
-export([handle_event/2, handle_info/2, handle_call/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3]).

%% PUBLIC API
%% GEN_SERVER CALLBACKS

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  Ref = market_events:subscribe(self()),
  {ok, Ref}.

handle_event(_Event, S) -> {ok, S}.

handle_info(_Msg, S) ->  lager:info("2"), {noreply, S}.

handle_call(_Msg, S) ->  {reply, ok, S}.

handle_call(_Msg, _, S) -> {reply, ok, S}.

handle_cast(_Msg, S) -> {noreply, S}.

terminate(Reason, S) ->
  lager:info("~p terminating due to ~p", [?MODULE, Reason]),
  market_events:unsubscribe(S),
  ok.

code_change(_, _, S) -> {ok, S}.
