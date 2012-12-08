-module(market_tests).

-export([test/0]).

-define(SYMBOLS, ["cocaine", "pot", "speed"]).


setup() ->
  {ok, Redis} = eredis:start_link(),
  eredis:q(Redis, ["FLUSHDB"]),
  eredis:q(Redis, ["HSET", "user:1", "cash", "10000000"]),
  eredis:q(Redis, ["HSET", "user:2", "cocaine", "10000000"]),
  eredis:q(Redis, ["HSET", "user:3", "cocaine", "10000000"]),
  eredis:q(Redis, ["HSET", "user:4", "cocaine", "10000000"]),
  eredis:q(Redis, ["HSET", "user:5", "cocaine", "10000000"]),
  eredis:q(Redis, ["HSET", "user:6", "cocaine", "10000000"]).


test() ->
  setup(),
  test_complex_fill().

test_simple_fill() ->
  market_broker:buy(1, cocaine, none, 100, none, cancelled),
  market_broker:sell(2, cocaine, none, 100, none, cancelled).

test_complex_fill() ->
  market_broker:sell(2, cocaine, 2, 10, none, cancelled),
  market_broker:sell(3, cocaine, 3, 25, none, cancelled),
  market_broker:sell(4, cocaine, 4, 100, none, cancelled),
  market_broker:sell(5, cocaine, 50, 100, none, cancelled),
  market_broker:sell(6, cocaine, 10, 500, none, cancelled),
  market_broker:buy(1, cocaine, none, 1000, none, cancelled).

test_partial_fill() ->
  market_broker:buy(1, cocaine, none, 1000, none, cancelled),
  market_broker:sell(2, cocaine, none, 500, none, cancelled).

test_limit_fill() ->
  market_broker:sell(2, cocaine, 10, 1000, none, cancelled),
  market_broker:buy(1, cocaine, none, 1000, none, cancelled).
