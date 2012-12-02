%% Handles bid, ask, close events on a market.
-module(market_events).
-export([start_link/0, subscribe/1, unsubscribe/1, bid/4, ask/4, close/3,
    cancel_bid/2, cancel_ask/2]).

-define(SERVER, {local, ?MODULE}).

start_link() ->
  gen_event:start_link(?SERVER).

subscribe(Listener) ->
  HandlerId = {market_event, make_ref()},
  gen_event:add_sup_handler(?MODULE, HandlerId, [Listener]),
  HandlerId.

unsubscribe(Listener) ->
  gen_event:delete_handler(?MODULE, Listener, []).

bid(From, Symbol, Price, Quantity) ->
  lager:info("~p bidding ~p for ~p", [From, Price, Symbol]),
  gen_event:notify(?MODULE, {bid, {From, Symbol, Price, Quantity}}).

ask(From, Symbol, Price, Quantity) ->
  lager:info("~p asking ~p for ~p", [From, Price, Symbol]),
  gen_event:notify(?MODULE, {ask, {From, Symbol, Price, Quantity }}).

close(Symbol, Price, Quantity) ->
  lager:info("~p closed at ~p", [Symbol, Price]),
  gen_event:notify(?MODULE, {close, {Symbol, Price, Quantity}}).

cancel_bid(From, Symbol) ->
  lager:info("~p canceled bid for ~p", [From, Symbol]),
  gen_event:notify(?MODULE, {bid_cancel, {From, Symbol}}).

cancel_ask(From, Symbol) ->
  lager:info("~p canceled ask for ~p", [From, Symbol]),
  gen_event:notify(?MODULE, {ask_cancel, {From, Symbol}}).
