-module(market_pub_proto).
-include("market_data.hrl").

-export([order/1]).

order(Order) ->
  #marketOrder {
    id=Id,
    symbol=Symbol,
    type=Type,
    limit=Limit,
    quantity=Quantity,
    quantity_constraint=QConst,
    time_in_force=Tif
  } = Order,
  Limit2 = case Limit of
    none -> <<"none">>;
    Limit -> Limit
  end,
  QConst2 = case QConst of
    none -> <<"none">>;
    QConst -> list_to_binary(atom_to_list(QConst))
  end,
  msgpack:pack({[
    {<<"type">>, <<"order">>},
    {<<"data">>, {[
          {<<"id">>, list_to_binary(Id)},
          {<<"symbol">>, list_to_binary(atom_to_list(Symbol))},
          {<<"type">>, list_to_binary(atom_to_list(Type))},
          {<<"limit">>, Limit2},
          {<<"quantity">>, Quantity},
          {<<"quantity_constraint">>, QConst2},
          {<<"time_in_force">>, list_to_binary(atom_to_list(Tif))}
  ]}}]}).

txn(Txn) ->
  #marketTxn {
    buy=Buy,
    sell=Sell,
    price=Price,
    quantity=Quantity
  } = Txn,
  msgpack:pack({[
    {<<"type">>, <<"txn">>},
    {<<"data">>, {[
          {<<"buy">>, list_to_binary(Buy)},
          {<<"sell">>, list_to_binary(Sell)},
          {<<"quantity">>, Quantity},
          {<<"price">>, Price}
  ]}}]}).

