%% Handles bid, ask, close events on a market.
-module(market_events).
-export([start_link/0, subscribe/1, unsubscribe/1, order_placed/1,
    order_filled/3, symbol_tick/3]).

-define(SERVER, {local, ?MODULE}).
-include("market_data.hrl").

start_link() ->
  gen_event:start_link(?SERVER).

subscribe(Listener) ->
  HandlerId = {market_event, make_ref()},
  gen_event:add_sup_handler(?MODULE, HandlerId, [Listener]),
  HandlerId.

unsubscribe(Listener) ->
  gen_event:delete_handler(?MODULE, Listener, []).

order_placed(Order) ->
  gen_event:notify(?MODULE, {order, Order}).

order_filled(Buy, Sell, Clear) ->
  gen_event:notify(?MODULE, {closed, Buy, Sell, Clear}).

symbol_tick(Symbol, New, Last) ->
  gen_event:notify(?MODULE, {tick, Symbol, New, Last}).
