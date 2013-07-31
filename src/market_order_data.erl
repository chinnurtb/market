-module(market_order_data).

-export([init/0]).
-export([bids/1, asks/1, orders/0]).
-export([save_order/1, get_order/1, get_contras/1, cancel_order/1]).

-include_lib("stdlib/include/qlc.hrl").
-include("market_data.hrl").

init() ->
  install().

install() ->
  mnesia:stop(),
  mnesia:create_schema([node()]),
  mnesia:start(),
  mnesia:create_table(marketOrder, [
      {attributes, record_info(fields, marketOrder)},
      {index, [#marketOrder.user, #marketOrder.symbol, #marketOrder.limit]},
      {disc_copies, [node()]},
      {type, set}
  ]).

bids(Symbol) ->
  F = fun() ->
      qlc:eval(qlc:q([O || O <- mnesia:table(marketOrder),
        O#marketOrder.symbol==Symbol,
        O#marketOrder.type==bid]))
  end,
  mnesia:activity(async_dirty, F).

asks(Symbol) ->
  F = fun() ->
      qlc:eval(qlc:q([O || O <- mnesia:table(marketOrder),
        O#marketOrder.symbol==Symbol,
        O#marketOrder.type==ask]))
  end,
  mnesia:activity(async_dirty, F).

orders() ->
  F = fun() ->
    qlc:eval(qlc:q([ O || O <- mnesia:table(marketOrder)]))
  end,
  R = mnesia:activity(async_dirty, F),
  lager:info("~p", [R]),
  R.

save_order(Order) ->
  F = fun() ->
      QH = qlc:q([ O || O <- mnesia:table(marketOrder),
        O#marketOrder.user==Order#marketOrder.user,
        O#marketOrder.symbol==Order#marketOrder.symbol,
        O#marketOrder.type==Order#marketOrder.type]),
      case qlc:eval(QH) of
        [] ->
          mnesia:write(Order);
        _ -> {abort, "existing order"}
      end
  end,
  mnesia:activity(transaction, F).

get_order(OrderId) ->
  F = fun() -> mnesia:read(marketOrder, OrderId) end,
  mnesia:activity(transaction, F).

get_contras(#marketOrder{user=User,type=Type,symbol=Symbol,limit=Limit}) ->
  CType = case Type of
    bid -> ask;
    ask -> bid
  end,
  F = fun() ->
    qlc:eval(qlc:q([ O || O <- mnesia:table(marketOrder),
        O#marketOrder.symbol==Symbol,
        O#marketOrder.type==CType,
        O#marketOrder.user/=User]))
  end,
  Ret = mnesia:activity(transaction, F),
  %% filter impossible limits
  Ret2 = lists:filter(fun(#marketOrder{limit=CLimit}) ->
    case CLimit of 
      0 -> true; %% all market contras
      CLimit -> %% limit contra
        case Limit of
          0 -> true; %% we are a market order
          Limit ->
            case Type of
              bid ->
                CLimit =< Limit;
              ask ->
                CLimit >= Limit
            end
        end
    end
  end, Ret),
  %% sort by limit then timestamp
  Ret3 = lists:sort(fun(A, B) ->
    case A#marketOrder.limit == B#marketOrder.limit of
      true ->
        A#marketOrder.timestamp =< B#marketOrder.timestamp;
      false ->
        A#marketOrder.limit =< B#marketOrder.limit
    end
  end, Ret2),
  lager:debug("CONTRAS: ~p", [Ret3]),
  Ret3.


cancel_order(OrderId) ->
  F = fun() -> mnesia:delete({marketOrder, OrderId}) end,
  mnesia:activity(transaction, F).
