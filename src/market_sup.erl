-module(market_sup).
-behaviour(supervisor).

-export([init/1, start_link/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Data = {market_data_sup, {market_data_sup, start_link, []}, transient, infinity, supervisor, [market_data_sup]},
    Broker = {market_broker, {market_broker, start_link, []}, permanent, 5000, worker, [market_broker]},
    {ok, { {one_for_one, 5, 10}, [Data, Broker]} }.
