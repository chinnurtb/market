-module(market_broker).
-behaviour(gen_server).

-export([start_link/0, init/1]).
-export([handle_event/2, handle_info/2, handle_call/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3]).

%% PUBLIC API
-export([fill_match/3]).

fill_match(Symbol, Bid, Ask) ->
  {closed, Amount, Price} = market_data:fill_order(Symbol, Bid, Ask),
  market_events:close(Symbol, Price, Amount),
  {closed, Amount, Price}.

%% GEN_SERVER CALLBACKS

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  {ok, []}.


handle_event(Event, S) ->
  lager:info("got ~p", [Event]),
  {ok, S}.

handle_info(Msg, S) ->
  lager:info("handle info got ~p", [Msg]),
  {noreply, S}.

handle_call(Msg, S) ->
  lager:info("handle_call/2 got ~p", [Msg]),
  {reply, ok, S}.

handle_call(Msg, _, S) ->
  lager:info("handle_call/3 got ~p", [Msg]),
  {reply, ok, S}.

handle_cast(Msg, S) ->
  lager:info("handle cast got ~p", [Msg]),
  {noreply, S}.

terminate(Reason, _) ->
  lager:info("~p terminating due to ~p", [?MODULE, Reason]),
  ok.

code_change(_, _, S) -> {ok, S}.

%%PRIVATE


