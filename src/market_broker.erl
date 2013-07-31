-module(market_broker).
-behaviour(gen_server).
-include("market_data.hrl").

-export([start_link/0, init/1]).
-export([handle_event/2, handle_info/2, handle_call/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3]).

%% PUBLIC API
-export([buy/1, buy/6, sell/1, sell/6, cancel/1, info/1]).

buy(#marketOrder{user=User, symbol=Symbol, limit=Limit, quantity=Quantity, quantity_constraint=QConst, time_in_force=Tif}) ->
  buy(User, Symbol, Limit, Quantity, QConst, Tif);

buy({User, Symbol, Limit, Quantity, QConst, Tif}) ->
  buy(User, Symbol, Limit, Quantity, QConst, Tif).

buy(User, Symbol, Limit, Quantity, QConst, Tif) ->
 lager:info("BUY CALLED~n~p ~p ~p ~p ~p ~p", [
     User, Symbol, Limit, Quantity, QConst, Tif]),
 gen_server:call(?MODULE, {order, #marketOrder {
    id=uuid:to_string(uuid:uuid4()),
    user=User,
    symbol=list_to_atom(val(Symbol)),
    type=bid,
    limit=val(Limit),
    quantity=val(Quantity),
    quantity_constraint=val(QConst),
    time_in_force=val(Tif),
    timestamp=market_utils:timestamp(),
    state=new,
    retries=0
  }}).

sell(#marketOrder{user=User, symbol=Symbol, limit=Limit, quantity=Quantity, quantity_constraint=QConst, time_in_force=Tif}) ->
  sell(User, Symbol, Limit, Quantity, QConst, Tif);
sell({User, Symbol, Limit, Quantity, QConst, Tif}) ->
  sell(User, Symbol, Limit, Quantity, QConst, Tif).

sell(User, Symbol, Limit, Quantity, QConst, Tif) ->
  gen_server:call(?MODULE, {order, #marketOrder {
        id=uuid:to_string(uuid:uuid4()),
    user=User,
    symbol=list_to_atom(val(Symbol)),
    type=ask,
    limit=val(Limit),
    quantity=val(Quantity),
    quantity_constraint=val(QConst),
    time_in_force=val(Tif),
    timestamp=market_utils:timestamp(),
    state=new,
    retries=0
  }}).

cancel(OrderId) ->
  market_order_data:cancel_order(OrderId).

info(OrderId) ->
  market_order_data:get_order(OrderId).

%% GEN_SERVER CALLBACKS

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  {ok, undefined_state}.

handle_event(_Event, S) -> {ok, S}.

handle_call(_Msg, S) -> {reply, ok, S}.

handle_call({order, Order}, _, S) ->
  Contras = market_order_data:get_contras(Order),
  Shit = market_maker:match_order(Order, Contras),
  Reply = case Shit of
    book ->
      case market_order_data:save_order(Order) of
        ok -> Order;
        Error -> {error, Error}
      end;
    {cancel, Reason} -> {cancelled, Reason};
    {execute, Group} ->
      execute_group(Order, Group);
    Error -> Error
  end,
  {reply, Reply, S};

handle_call(_Msg, _, S) -> {reply, ok, S}.

handle_cast(_Msg, S) -> {noreply, S}.

handle_info(_Msg, S) -> {noreply, S}.

terminate(Reason, _) ->
  lager:debug("~p terminating due to ~p", [?MODULE, Reason]),
  ok.

code_change(_, _, S) -> {ok, S}.

execute_group(Order, {QuantityFilled, Group}) ->
  lager:debug("GROUP IS ~p", [Group]),
  Ret = execute_group_member(Order, Group, Order#marketOrder.quantity, []),
  case Ret of
    {closed, _, Txns} ->
      lager:debug("EXECUTE RET: ~p", [Ret]),
      lists:foreach(fun({O, _}) ->
        market_order_data:delete(O#marketOrder.id)
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
          A = case Order#marketOrder.type of
            bid -> buy;
            ask -> sell
          end,
          erlang:spawn(market_broker, A, [O2])
      end;
    _ -> true
  end,
  Ret.

execute_group_member(O, [], _, R) -> {closed, O, R};
execute_group_member(O, [ H | T ], Q, R) ->
  case execute_pair(O, H) of
    {closed, {_, Quantity} = Tx} ->
      R2 = R ++ [{H, Tx}],
      Q2 = Q - Quantity,
      execute_group_member(O, T, Q2, R2);
    {cancelled, Reason, none} ->
      {cancelled, Reason};
    {cancelled, Reason, BadOrderer} ->
      lager:error("ERROR: ~p", [Reason]),
      lager:error("BAD ORDER: ~p", [BadOrderer]),
      [BadO|_] = lists:filter(fun(#marketOrder{user=User}) ->
        User == BadOrderer
      end, [O, H]),
      market_order_data:delete(BadO#marketOrder.id),
      execute_group_member(O, T, Q, R)
  end.

execute_pair(B, S) ->
  P = trade_price(B, S),
  Q = trade_quantity(B, S),
  market_data:execute(B, S, P, Q).
  
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

val(A) -> market_utils:val(A).

