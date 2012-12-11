-module(market_utils).

-export([timestamp/0, zip/2, zip_proplist/2, plural/1]).

timestamp() ->
  {Mega, Secs, _} = os:timestamp(),
  Mega*1000000+Secs.

%% TAKES TWO LISTS
%% A GENERIC LIST OF SYMBOL DATA
%% AND A LIST OF SYMBOLS
%% AND "ZIPS" THEM TOGETHER INTO A DICT
zip(L, S) -> zip(L, S, dict:new()).
zip(_, [], R) -> R;
zip([], _, R) -> R;
zip([O | OT], [S | ST], R) ->
  zip(OT, ST, dict:store(S, O, R)).

zip_proplist(L, S) -> zip_proplist(L, S, []).
zip_proplist(_, [], R) -> R;
zip_proplist([ H | T ], [ S | ST ], R) ->
  zip_proplist(T, ST, R ++ [{S, H}]).

plural(ask) -> asks;
plural(bid) -> bids;
plural(T) -> T.
