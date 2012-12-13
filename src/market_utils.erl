-module(market_utils).

-export([timestamp/0, zip/2, zip_proplist/2, plural/1, val/1]).

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


val(A) ->
  S = btl(A),
  Ret = case string:to_integer(S) of
    {Int, []} -> Int;
    _ -> S
  end,
  case Ret of
    "none" -> none;
    "all" -> all;
    "cancelled" -> cancelled;
    "bid" -> bid;
    "ask" -> ask;
    "day" -> day;
    "immediate" -> immediate;
    "fill" -> fill;
    "closed" -> closed;
    "new" -> new;
    "booked" -> booked;
    "locked" -> locked;
    _ -> Ret
  end.

btl(B) when is_binary(B) -> binary_to_list(B);
btl(B) when is_list(B) -> B;
btl(B) when is_integer(B) -> integer_to_list(B);
btl(B) -> lager:error("WEIRD TYPE ~p", [B]), B.

