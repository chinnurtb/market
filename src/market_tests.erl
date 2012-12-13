-module(market_tests).

-export([test/0, test_forever/0, test_random/0]).

-include("market_data.hrl").

setup() ->
  {ok, Redis} = eredis:start_link(),
  gen_server:cast(market_orders, reload),
  gen_server:cast(limit_orders, reload),
  eredis:q(Redis, ["HSET", "user:1", "cash", "10000000000"]),
  eredis:q(Redis, ["HSET", "user:2", "cocaine", "100000000000"]),
  eredis:q(Redis, ["HSET", "user:3", "cocaine", "100000000000"]),
  eredis:q(Redis, ["HSET", "user:4", "cocaine", "100000000000"]),
  eredis:q(Redis, ["HSET", "user:5", "cocaine", "100000000000"]),
  eredis:q(Redis, ["HSET", "user:6", "cocaine", "100000000000"]).

test() ->
  setup(),
  test_simple_fill(),
  test_complex_fill(),
  test_partial_fill(),
  test_limit_fill().

test_forever() ->
  [ erlang:spawn(?MODULE, test_random, []) || _ <- lists:seq(1, 20000) ].

test_random() ->
  random:seed(erlang:now()),
  User = random:uniform(6),
  Symbol = lists:nth(random:uniform(10), ?SYMBOLS),
  Q = random:uniform(10000),
  L = random:uniform(1000) - 1,
  L2 = case L of
    0 -> none;
    _ -> L
  end,
  case random:uniform(2) of
    1 ->
      market_broker:buy(User, Symbol, L2, Q, none, cancelled);
    _ ->
      market_broker:sell(User, Symbol, L2, Q, none, cancelled)
  end,
  erlang:spawn(?MODULE, test_random, []).

test_simple_fill() ->
  market_broker:buy(1, cocaine, none, 100, none, cancelled),
  market_broker:sell(2, cocaine, none, 100, none, cancelled).

test_complex_fill() ->
  market_broker:sell(2, cocaine, 2, 10, none, cancelled),
  market_broker:sell(3, cocaine, 3, 25, none, cancelled),
  market_broker:sell(4, cocaine, 4, 100, none, cancelled),
  market_broker:sell(5, cocaine, 50, 100, all, cancelled),
  market_broker:sell(6, cocaine, 10, 500, none, cancelled),
  market_broker:buy(1, cocaine, none, 1000, none, cancelled).

test_partial_fill() ->
  market_broker:buy(1, cocaine, none, 1000, none, cancelled),
  market_broker:sell(2, cocaine, none, 500, none, cancelled).

test_limit_fill() ->
  market_broker:sell(2, cocaine, 10, 1000, all, cancelled),
  market_broker:buy(1, cocaine, none, 1001, none, cancelled).
