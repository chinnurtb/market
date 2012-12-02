-module(market_tests).

-export([test/0, test2/0]).

-define(SYMBOLS, ["cocaine", "pot", "speed"]).

test() ->
  [spawn(?MODULE, test2, []) || _ <- lists:seq(1, 4000)].

test2() ->
  random:seed(erlang:now()),
  User = random:uniform(100000),
  Amount = random:uniform(1000),
  Symbol = lists:nth(random:uniform(3), ?SYMBOLS),
  Res = case random:uniform(2) of
    1 ->
      market_broker:sell(User, Symbol, none, Amount, none, canceled);
    2 ->
      market_broker:buy(User, Symbol, none, Amount, none, canceled)
  end,
  timer:sleep(random:uniform(10) * 100),
  spawn(?MODULE, test2, []).
