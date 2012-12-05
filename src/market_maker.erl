-module(market_maker).
-behaviour(gen_server).
-include("market_data.hrl").

-export([start_link/0, init/1]).
-export([handle_event/2, handle_info/2, handle_call/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3]).

%% PUBLIC API
%% GEN_SERVER CALLBACKS

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  Ref = market_events:subscribe(self()),
  {ok, Ref}.

handle_event(_Event, S) -> {ok, S}.

handle_info({order, Order}, S) ->
  lager:info("MAKER GOT ORDER ~p~n~n", [Order]),
  Result = case is_valid(Order) of
    true -> make_market(Order);
    false -> cancelled
  end,
  lager:info("MATCH RESULT: ~p~n~n", [Result]),
  {noreply, S};

handle_info({tick, Symbol, New, Last}, S) ->
  rematch(Symbol, New, Last),
  {noreply, S};

handle_info(_Msg, S) -> {noreply, S}.

handle_call(_Msg, S) -> {reply, ok, S}.

handle_call(_Msg, _, S) -> {reply, ok, S}.

handle_cast(_Msg, S) -> {noreply, S}.

terminate(Reason, S) ->
  lager:info("~p terminating due to ~p", [?MODULE, Reason]),
  market_events:unsubscribe(S),
  ok.

code_change(_, _, S) -> {ok, S}.

is_valid(Order) ->
  case Order#marketOrder.limit of
    none ->
      market_orders:order_exists(Order);
    _ ->
      limit_orders:order_exists(Order)
  end.

make_market(Order) ->
  case match_order(Order) of
    {filled, Orders} ->
      execute_order(Order, Orders);
    {partial, _Orders} ->
      case Order#marketOrder.time_in_force of
        fill ->
          market_broker:cancel(Order#marketOrder.id);
        _ ->
          lager:info("PARTIAL, BAILING"),
          %% EXECUTE PARTIAL

          case Order#marketOrder.time_in_force of
            immediate -> cancel;
            _ ->
              ok
              %% RESUBMIT REMAINDER
          end
      end;
    {cancel, []} ->
      lager:info("CANCEL ORDER"),
      market_broker:cancel(Order#marketOrder.id)
  end.


match_order(#marketOrder{symbol=Symbol, type=Type} = Order) ->
  lager:info("MATCHING: ~p~n~n", [Order]),
  MarketContras = case Type of
    bid ->
      market_orders:get_asks(Symbol);
    ask ->
      market_orders:get_bids(Symbol)
  end,
  lager:info("MARKET CONTRAS: ~p~n~n", [MarketContras]),
  SimpleContras = get_simple_market_contras(Order, MarketContras),
  lager:info("SIMPLE CONTRAS: ~p~n~n", [SimpleContras]),
  QMatch = Order#marketOrder.quantity,
  Group = case accumulate_quantity(Order, SimpleContras) of
    {0, []} ->
      {cancel, []};
    {QMatch, Orders} ->
      {filled, Orders};
    {Quantity, Orders} ->
      {partial, Quantity, Orders}
  end,
  lager:info("GROUP: ~p~n~n", [Group]),
  Group.

%% CANT FILL YOUR OWN ORDERS
%% ONLY WANT NON All-Or-Nothing
get_simple_market_contras(Order, Contras) ->
  lists:filter(fun(C) ->
    B1 = C#marketOrder.user /= Order#marketOrder.user,
    B2 = C#marketOrder.quantity_constraint == none,
    B1 and B2
  end, Contras).

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
      accumulate_quantity(O, [], L ++ [H], O#marketOrder.quantity);
    false ->
      accumulate_quantity(O, T, L ++ [H], Count + H#marketOrder.quantity)
  end.

execute_order(O, Contras) when is_list(Contras) ->
  execute_order(O, Contras, []).

execute_order(_, [], R) -> R;
execute_order(O, [ H | T ], R) ->
  Price = execution_price(O, H),
  Quantity = execution_quantity(O, H),
  lager:info("ATTEMPTING TO EXECUTE ~p AGAINST ~p", [O, H]),
  lager:info("Q: ~p P:~p", [Quantity, Price]),
  Result = execute_order(O, H, Price, Quantity),
  lager:info("EXECUTE RESULT: ~p~n", [Result]),
  execute_order(O, T, R ++ [Result]).

execute_order(O, C, Price, Quantity) ->
  Res = market_data:execute(O#marketOrder.id, C#marketOrder.id, Price, Quantity),
  Res.


execution_price(O, C) ->
  case C#marketOrder.limit of
    none -> market_data:quote(O#marketOrder.symbol);
    _ -> C#marketOrder.limit
  end.

execution_quantity(O, C) ->
  OQ = O#marketOrder.quantity,
  CQ = C#marketOrder.quantity,

  case OQ > CQ of
    true -> CQ;
    false -> OQ
  end.

%% SIMPLE QUANTITY CONSTRAINT CHECK
%% FILTER CONTRAS THAT HAVE QCONST
%% THAT WE CANT MATCH
%filter_contras_quantity(Order, Contras) ->
%  C2 = lists:filter(fun(C) ->
%    #marketOrder{quantity=Quantity,
%      quantity_constraint=QConst} = Order,
%    #marketOrder{quantity=Quantity2,
%      quantity_constraint=QConst2} = C,
%    QC1 = case QConst of
%      all -> Quantity > Quantity2;
%      _ -> true
%    end,
%    QC2 = case QConst2 of
%      all -> Quantity2 > Quantity;
%      _ -> true
%    end,
%    QC2 and QC1
%  end, Contras),
%  C2.

%% A SYMBOL TICKED
%% SO CHECK OUR LIMIT ORDERS
%% FOR FILLABLES!
rematch(_Symbol, _New, _Last) -> ok.
