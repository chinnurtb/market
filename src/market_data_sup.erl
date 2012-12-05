-module(market_data_sup).
-behaviour(supervisor).

-export([init/1, start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Data = {market_data, {market_data, start_link, []}, permanent, 5000, worker, [market_data]},
    MarketOrders = {market_market_orders, {market_orders, start_link, [market]}, permanent, 5000, worker, [market_orders]},
    LimitOrders = {market_limit_orders, {market_orders, start_link, [limit]}, permanent, 5000, worker, [market_orders]},
    OrderQueue = {market_order_queue, {market_order_queue, start_link, []}, permanent, 5000, worker, [market_order_queue]},
    {ok, { {rest_for_one, 5, 10}, [Data, MarketOrders, LimitOrders, OrderQueue]} }.
