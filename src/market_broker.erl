-module(market_broker).
-behaviour(gen_server).
-include("market_data.hrl").

-export([start_link/0, init/1]).
-export([handle_event/2, handle_info/2, handle_call/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3]).

%% PUBLIC API
-export([buy/6, sell/6, cancel/1]).
%% "PROTECTED" LEVEL2 DATA!!
-export([
    bids/1, asks/1,
    market_bids/1, limit_bids/1,
    market_asks/1, limit_asks/1,
    book_order/1
]).

buy(User, Symbol, Limit, Quantity, QConst, Tif) ->
 gen_server:call(?MODULE, {order, #marketOrder {
    id=uuid:to_string(uuid:uuid4()),
    user=User,
    symbol=Symbol,
    type=bid,
    limit=Limit,
    quantity=Quantity,
    quantity_constraint=QConst,
    time_in_force=Tif,
    timestamp=market_utils:timestamp(),
    state=new
  }}).

sell(User, Symbol, Limit, Quantity, QConst, Tif) ->
  gen_server:call(?MODULE, {order, #marketOrder {
    id=uuid:to_string(uuid:uuid4()),
    user=User,
    symbol=Symbol,
    type=ask,
    limit=Limit,
    quantity=Quantity,
    quantity_constraint=QConst,
    time_in_force=Tif,
    timestamp=market_utils:timestamp(),
    state=new
  }}).

cancel(OrderId) -> cancel_order(OrderId, "Cancelled By User").

bids(Symbol) ->
  market_bids(Symbol) ++ limit_bids(Symbol).
asks(Symbol) ->
  market_asks(Symbol) ++ limit_asks(Symbol).

market_bids(Symbol) ->
  gen_server:call(market_orders, {Symbol, bids}).

market_asks(Symbol) ->
  gen_server:call(market_orders, {Symbol, asks}).

limit_bids(Symbol) ->
  gen_server:call(limit_orders, {Symbol, bids}).

limit_asks(Symbol) ->
  gen_server:call(limit_orders, {Symbol, asks}).

book_order(Order) ->
  lager:info("book_order"),
  Order2 = Order#marketOrder {
    state=booked
  },
  case Order#marketOrder.limit of
    none ->
      gen_server:call(market_orders, {book, Order2});
    _ ->
      gen_server:call(limit_orders, {book, Order2})
  end.

%% GEN_SERVER CALLBACKS

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  State = dict:new(),
  {ok, State}.

handle_event(_Event, S) -> {ok, S}.

handle_call(_Msg, S) -> {reply, ok, S}.

handle_call({order, Order}, From, S) ->
  market_order_queue:push(Order),
  S2 = dict:store(Order#marketOrder.id, From, S),
  {reply, {queued, Order#marketOrder.id}, S2};

handle_call(_Msg, _, S) -> {reply, ok, S}.

handle_cast({execute, Order, Group}, S) ->
  {From, _} = dict:fetch(Order#marketOrder.id, S),
  From ! {executing, Order, Group},
  Result = execute_group(Order, Group, From),
  {noreply, S};

handle_cast({cancel, Order, Reason}, S) ->
  {From, _} = dict:fetch(Order#marketOrder.id, S),
  From ! {cancelled, Order, Reason},
  {noreply, S};

handle_cast({book, Order}, S) ->
  lager:info("book"),
  {From, _} = dict:fetch(Order#marketOrder.id, S),
  case book_order(Order) of
    {error, Reason} ->
      lager:info("BOOKING ERROR: ~p", [Reason]),
      From ! {error, Order, Reason};
    {saved, Order2} ->
      lager:info("BOOKED ~p", [Order2]),
      From ! {booked, Order2}
  end,
  {noreply, S};

handle_cast(_Msg, S) -> {noreply, S}.

handle_info(_Msg, S) -> {noreply, S}.

terminate(Reason, _) ->
  lager:info("~p terminating due to ~p", [?MODULE, Reason]),
  ok.

code_change(_, _, S) -> {ok, S}.

cancel_order(OrderId, Reason) ->
  case market_data:get_order(OrderId) of
    none -> {error, not_found};
    Order ->
      case Order#marketOrder.limit of
        none ->
          gen_server:call(market_orders, {cancel, Order, Reason});
        _ ->
          gen_server:call(limit_orders, {cancel, Order, Reason})
      end
  end.

delete_order(OrderId) ->
  case market_data:get_order(OrderId) of
    none -> {error, not_found};
    Order ->
      case Order#marketOrder.limit of
        none ->
          gen_server:call(market_orders, {delete, Order});
        _ ->
          gen_server:call(limit_orders, {delete, Order})
      end
  end.

close_order(Order, Txn) ->
  case Order#marketOrder.limit of
    none ->
      gen_server:call(market_orders, {close, Order, Txn});
    _ ->
      gen_server:call(limit_orders, {close, Order, Txn})
  end.

execute_group(Order, {QuantityFilled, Group}, From) ->
  Ret = execute_group_member(Order, Group, Order#marketOrder.quantity, []),
  lager:info("EXECUTE RET: ~p", [Ret]),
  From ! Ret.

execute_group_member(O, [], _, R) -> {O, R};
execute_group_member(O, [ H | T ], Q, R) ->
  {Buy, Sell} = case O#marketOrder.type of
    bid ->
      {O, H};
    ask ->
      {H, O}
  end,
  case execute_pair(Buy, Sell) of
    {closed, Tx} ->
      R2 = R ++ [{H, Tx}],
      Q2 = Q - Tx#marketTxn.quantity,
      execute_group_member(O, T, Q2, R2);
    {cancelled, Reason, BadOrder} ->
      lager:error("ERROR: ~p", [Reason]),
      lager:error("BAD ORDER: ~p", [BadOrder]),
      %% tag order if it's the bad one
      %% so we know not to requeue at the end of
      %% the rollback
      O2 = case O#marketOrder.id of
        BadOrder -> 
          O#marketOrder{state=cancelled};
        _ -> O
      end,
      case Reason of 
        "locked" -> ok; %% DO NOTHING
        _ ->
          cancel_order(BadOrder, Reason)
      end,
      rollback(O2, R)
  end.

rollback(O, []) ->
  case O#marketOrder.quantity_constraint of
    all ->
      lager:info("rollback cancel"),
      {cancelled, O, all};
    _ ->
      %% REQUEUE ORDER
      case O#marketOrder.state of
        cancelled -> {cancelled, "NON SUFFICIENT FUNDS", O};
        _ ->
          lager:info("rollback requeue"),
          spawn(fun() ->
            gen_server:call(?MODULE, {order, O})
          end),
          {requeued, O}
      end
  end;

rollback(O, T) -> rollback(O, T, []).

rollback(O, [ H | T ], R) ->
  lager:info("roll back ~p ~p", [O, R]),
  R2 = R ++ rollback_transaction(O, H),
  rollback(O, T, R2).

execute_pair(B, S) ->
  P = trade_price(B, S),
  Q = trade_quantity(B, S),
  market_data:execute(B, S, P, Q).
  
rollback_transaction(Order, Contra) -> ok.

trade_quantity(B, S) ->
  case B#marketOrder.quantity >=
    S#marketOrder.quantity of
    true -> S#marketOrder.quantity;
    false -> B#marketOrder.quantity
  end.

trade_price(#marketOrder{symbol=Symbol, limit=BL},
  #marketOrder{limit=SL}) ->
  case BL of
    none ->
      case SL of
        none -> market_data:quote(Symbol);
        _ -> SL
      end;
    _ ->
      case SL of
        none -> market_data:quote(Symbol);
        _ -> BL
      end
  end.
