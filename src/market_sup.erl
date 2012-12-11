-module(market_sup).
-behaviour(supervisor).

-export([init/1, start_link/0]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Data = {market_data_sup, {market_data_sup, start_link, []}, transient, infinity, supervisor, [market_data_sup]},
    Events = {market_events, {market_events, start_link, []}, permanent, 5000, worker, [market_events]},
    Broker = {market_broker, {market_broker, start_link, []}, permanent, 5000, worker, [market_broker]},
    Makers = {market_maker_sup, {market_maker_sup, start_link, []}, transient, infinity, supervisor, [market_maker_sup]},
    Ticker = {market_ticker, {market_ticker, start_link, []}, permanent, 1000, worker, [market_ticker]},
    {ok, { {one_for_one, 10, 60}, [
      Data, Events, Broker, Ticker, Makers
    ]} }.
