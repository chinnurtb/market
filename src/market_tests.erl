-module(market_tests).

-export([test/0, test2/0]).

-define(SYMBOLS, ["cocaine", "pot", "speed", "acid", "shrooms", "oxy", "valium", "pcp", "hash", "percocet", "mdma"]).

test() ->
  [spawn(?MODULE, test2, []) || _ <- lists:seq(1, 100)].

test2() ->
  random:seed(erlang:now()),
  User = random:uniform(10000),
  Amount = random:uniform(1),
  case random:uniform(2) of
    1 ->
      market_broker:sell(User, "coke", none, Amount, none, canceled);
    2 ->
      market_broker:buy(User, "coke", none, Amount, none, canceled)
  end.
