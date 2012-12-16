-module(market_broker).
-behaviour(gen_server).
-include("market_data.hrl").

-export([start_link/0, init/1]).
-export([handle_event/2, handle_info/2, handle_call/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3]).

%% PUBLIC API
-export([buy/1, buy/6, sell/1, sell/6, cancel/1]).
%% "PROTECTED" LEVEL2 DATA!!
-export([
    bids/1, asks/1,
    market_bids/1, limit_bids/1,
    market_asks/1, limit_asks/1,
    book_order/1,
    refresh_books/0
]).
buy({User, Symbol, Limit, Quantity, QConst, Tif}) ->
  buy(User, Symbol, Limit, Quantity, QConst, Tif).

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
    state=new,
    retries=0
  }}).

sell({User, Symbol, Limit, Quantity, QConst, Tif}) ->
  sell(User, Symbol, Limit, Quantity, QConst, Tif).

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
    state=new,
    retries=0
  }}).

cancel(OrderId) ->
  cancel_order(OrderId, "Cancelled By User"),
  self() ! {cancelled, OrderId, "Cancelled By User"}.

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
  Order2 = Order#marketOrder {
    state=booked
  },
  Ret = case Order#marketOrder.limit of
    none ->
      gen_server:call(market_orders, {book, Order2});
    _ ->
      gen_server:call(limit_orders, {book, Order2})
  end,
  lager:info("BOOK: ~p", [Ret]),
  case Ret of
    {error, {rejected, _}} ->
      lager:info("YES"),
      market_data:delete_order(Order#marketOrder.id),
      Ret;
    Ret -> Ret
  end.

refresh_books() ->
  gen_server:cast(market_orders, refresh),
  gen_server:cast(limit_orders, refresh).

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
  case Order#marketOrder.retries > 10 of
    true ->
      lager:error("TOO MANY RETRIES ~p", [Order]),
      S2 = dict:erase(Order#marketOrder.id, S),
      {reply, {cancelled, "TOO MANY RETRIES"}, S2};
    false ->
      Retries = Order#marketOrder.retries,
      Order2 = Order#marketOrder{retries=Retries+1},
      market_order_queue:push(Order2),
      S2 = dict:store(Order#marketOrder.id, From, S),
      {reply, {queued, Order#marketOrder.id}, S2}
  end;

handle_call(_Msg, _, S) -> {reply, ok, S}.

handle_cast({execute, Order, Group}, S) ->
  Id = Order#marketOrder.id,
  notify_orderer(Id, {executing, Order, Group}, S),
  Ret = execute_group(Order, Group),
  lager:info("EXECUTE GROUP RET: ~p", [Ret]),
  notify_orderer(Id, Ret, S),
  S2 = clear_orderer(Id, S),
  {noreply, S2};

handle_cast({cancel, Order, Reason}, S) ->
  Id = Order#marketOrder.id,
  notify_orderer(Id, {cancelled, Order, Reason}, S),
  S2 = clear_orderer(Id, S),
  {noreply, S2};

handle_cast({book, Order}, S) ->
  Id = Order#marketOrder.id,
  case book_order(Order) of
    {error, Reason} ->
      lager:debug("BOOKING ERROR: ~p", [Reason]),
      notify_orderer(Id, {error, Order, Reason}, S);
    {saved, Order2} ->
      lager:debug("BOOKED ~p", [Order2]),
      notify_orderer(Id, {booked, Order2}, S)
  end,
  S2 = clear_orderer(Id, S),
  {noreply, S2};

handle_cast(_Msg, S) -> {noreply, S}.

handle_info(_Msg, S) -> {noreply, S}.

terminate(Reason, _) ->
  lager:debug("~p terminating due to ~p", [?MODULE, Reason]),
  ok.

code_change(_, _, S) -> {ok, S}.

cancel_order(OrderId, Reason) ->
  case market_data:get_order(OrderId) of
    none -> {error, not_found};
    Order ->
      case Order#marketOrder.limit of
        none ->
          gen_server:cast(market_orders, {cancel, Order, Reason});
        _ ->
          gen_server:cast(limit_orders, {cancel, Order, Reason})
      end
  end.

close_order(Order, Txn) ->
  case Order#marketOrder.limit of
    none ->
      gen_server:cast(market_orders, {close, Order, Txn});
    _ ->
      gen_server:cast(limit_orders, {close, Order, Txn})
  end.

execute_group(Order, {QuantityFilled, Group}) ->
  lager:debug("GROUP IS ~p", [Group]),
  GroupLock = uuid:to_string(uuid:uuid4()),
  Ret = execute_group_member(GroupLock, Order, Group, Order#marketOrder.quantity, []),
  case Ret of
    {closed, _, Txns} ->
      lager:debug("EXECUTE RET: ~p", [Ret]),
      %% make sure order is written so
      lists:foreach(fun({O, X}) ->
        close_order(O,X)
      end, Txns),
      Diff =  Order#marketOrder.quantity - QuantityFilled,
      case Diff of
        0 -> true; %% NO OP
        _ -> 
          O2 = Order#marketOrder{
            quantity=Diff,
            timestamp=market_utils:timestamp(),
            id=uuid:to_string(uuid:uuid4()),
            state=new
          },
          market_order_queue:push(O2)
      end;
    _ -> true
  end,
  Ret.

execute_group_member(_, O, [], _, R) -> {closed, O, R};
execute_group_member(Lock, O, [ H | T ], Q, R) ->
  case execute_pair(Lock, O, H) of
    {closed, Tx} ->
      R2 = R ++ [{H, Tx}],
      Q2 = Q - Tx#marketTxn.quantity,
      execute_group_member(Lock, O, T, Q2, R2);
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
        locked -> true; %% DO NOTHING
        _ ->
          case BadOrder of
            none -> true;
            _ -> cancel_order(BadOrder, Reason)
          end
      end,
      rollback(O2, R);
    _ ->
      rollback(O, R)
  end.

rollback(O, []) ->
  case O#marketOrder.quantity_constraint of
    all ->
      lager:debug("ROLLBACK CANCEL ~p", [O]),
      {cancelled, O, all};
    _ ->
      %% REQUEUE ORDER
      case O#marketOrder.state of
        cancelled -> {cancelled, "NON SUFFICIENT FUNDS", O};
        _ ->
          spawn(fun() ->
            lager:debug("ROLLBACK REQUEUE ~p", [O]),
            gen_server:call(?MODULE, {order, O})
          end),
          {requeued, O}
      end
  end;

rollback(O, T) -> rollback(O, T, []).

rollback(_, [], R) -> R;
rollback(O, [ H | T ], R) ->
  R2 = R ++ [rollback_transaction(H)],
  rollback(O, T, R2).

execute_pair(L, B, S) ->
  P = trade_price(B, S),
  Q = trade_quantity(B, S),
  market_data:execute(L, B, S, P, Q).
  
rollback_transaction({_, Txn}) ->
  lager:debug("ROLLING BACK TRANSACTION ~p", [Txn]),
  market_data:rollback(Txn).

trade_quantity(B, S) ->
  case B#marketOrder.quantity >=
    S#marketOrder.quantity of
    true -> S#marketOrder.quantity;
    false -> B#marketOrder.quantity
  end.

trade_price(#marketOrder{symbol=Symbol, limit=BL},
  #marketOrder{limit=SL}) ->
  BL2 = case BL of
    none -> 0;
    _ -> BL
  end,
  SL2 = case SL of
    none -> 0;
    _ -> SL
  end,
  P = case BL2 >= SL2 of
    true -> BL2;
    false -> SL2
  end,
  case P of
    0 -> market_data:quote(Symbol);
    _ -> P
  end.
  %case BL of
  %  none ->
  %    case SL of
  %      none -> market_data:quote(Symbol);
  %      _ -> SL
  %    end;
  %  _ ->
  %    case SL of
  %      none -> market_data:quote(Symbol);
  %      _ -> BL
  %    end
  %end.

notify_orderer(Id, Msg, S) ->
  case dict:is_key(Id, S) of
    true ->
      {From, _} = dict:fetch(Id, S),
      From ! Msg,
      From;
    false -> ok
  end.

clear_orderer(Id, S) ->
  case dict:is_key(Id, S) of
    true -> dict:erase(Id, S);
    false -> S
  end.
