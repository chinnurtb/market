-module(market_utils).

-export([timestamp/0]).

timestamp() ->
  {Mega, Secs, _} = now(),
  Mega*1000000+Secs.
