-module(market_sup).
-behaviour(supervisor).

-export([init/1, start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Market = {market_events, {market_events, start_link, []}, permanent, 5000, worker, [market_events]},
    Data = {market_data, {market_data, start_link, []}, permanent, 5000, worker, [market_data]},
    Broker = {market_broker, {market_broker, start_link, []}, permanent, 5000, worker, [market_broker]},
    {ok, { {rest_for_one, 5, 10}, [Data, Market, Broker]} }.
