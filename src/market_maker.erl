-module(market_maker).
-include("market_data.hrl").

%% PUBLIC API
-export([match_order/2]).

match_order(Order, Contras) ->
  lager:debug("MATCHING: ~p~n~n", [Order]),
  Tif = Order#marketOrder.time_in_force,
  Group = accumulate_quantity(Order, Contras),
  Ret = case Group of
    {0, []} ->
      case Tif of
        immediate -> {cancel, immediate};
        fill -> {cancel, fill};
        _ -> book
      end;
    {Q, _} -> 
      case Q < Order#marketOrder.quantity of
        true ->
          case Tif of
            fill -> {cancel, fill};
            _ ->
              {execute, Group}
          end;
        _ -> {execute, Group}
      end
  end,
  lager:debug("MATCH RESULT: ~p", [Ret]),
  Ret.

accumulate_quantity(O, C) ->
  accumulate_quantity(O, C, [], 0).

accumulate_quantity(_, [], List, Count) ->
  {Count, List};
accumulate_quantity(#marketOrder{quantity=Q}, _, L, Count) when
  Count >= Q -> {Q, L};
accumulate_quantity(O, [ H | T ], L, Count) ->
  LookingFor = O#marketOrder.quantity - Count,
  case H#marketOrder.quantity >= LookingFor of
    true ->
      %% IS THIS AN AON CONTRA?
      case quantity_compatible(LookingFor, H) of
        true ->
          accumulate_quantity(O, [], L ++ [H], O#marketOrder.quantity);
        false ->
          accumulate_quantity(O, T, L, Count)
      end;
    false ->
      accumulate_quantity(O, T, L ++ [H], Count + H#marketOrder.quantity)
  end.

quantity_compatible(Quantity, Contra) ->
  case Contra#marketOrder.quantity_constraint of
    all -> Quantity >= Contra#marketOrder.quantity_constraint;
    _ -> true
  end.
