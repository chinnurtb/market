-module(market_tests).

-export([test/0]).

-define(SYMBOLS, ["cocaine", "pot", "speed"]).

test() ->
  test_simple_fill().

test_simple_fill() ->
  market_broker:buy(1, cocaine, none, 100, none, cancelled),
  market_broker:sell(2, cocaine, none, 100, none, cancelled).

test_partial_fill() ->
  market_broker:buy(1, cocaine, none, 1000, none, cancelled),
  market_broker:sell(2, cocaine, none, 500, none, cancelled).
