%% Handles bid, ask, close events on a market.
-module(market_events).
-export([start_link/0, subscribe/1, unsubscribe/1, order_placed/1,
    order_closed/1, order_cancelled/1]).

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

order_closed(Txn) ->
  gen_event:notify(?MODULE, {closed, Txn}).

order_cancelled(Order) ->
  gen_event:notify(?MODULE, {cancelled, Order}).
