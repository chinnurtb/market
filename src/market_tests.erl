-module(market_tests).

-export([test/0]).

-define(SYMBOLS, ["cocaine", "pot", "speed"]).

test() ->
  test_simple_fill().

test_simple_fill() ->
  market_broker:buy(1, cocaine, none, 100, none, cancelled),
  %% pause so it gets booked
  timer:sleep(100),
  market_broker:sell(2, cocaine, none, 100, none, cancelled).

test_complex_fill() ->
  market_broker:sell(2, cocaine, none, 10, none, cancelled),
  market_broker:sell(3, cocaine, none, 25, none, cancelled),
  market_broker:sell(4, cocaine, none, 100, none, cancelled),
  market_broker:sell(5, cocaine, none, 100, none, cancelled),
  market_broker:sell(6, cocaine, none, 500, none, cancelled),
  market_broker:buy(1, cocaine, none, 1000, none, fill),
  market_broker:buy(1, cocaine, none, 1000, none, cancelled).

test_partial_fill() ->
  market_broker:buy(1, cocaine, none, 1000, none, cancelled),
  market_broker:sell(2, cocaine, none, 500, none, cancelled).
