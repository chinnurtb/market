-module(market_maker).
-behaviour(gen_server).
-include("market_data.hrl").

-export([start_link/0, init/1]).
-export([handle_event/2, handle_info/2, handle_call/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3]).

%% PUBLIC API
-export([match_order/1]).

match_order(Order) ->
  lager:info("MATCHING: ~p~n~n", [Order]),
  Symbol = Order#marketOrder.symbol,
  Tif = Order#marketOrder.time_in_force,
  Contras = case Order#marketOrder.type of
    bid ->
      market_broker:asks(Symbol);
    ask ->
      market_broker:bids(Symbol)
  end,
  Group = accumulate_quantity(Order, Contras),
  Ret = case Group of
    {0, []} ->
      case Tif of
        immediate -> {cancel, Order, immediate};
        fill -> {cancel, Order, fill};
        _ ->
          {book, Order}
      end;
    _ -> {execute, Order, Group}
  end,
  lager:info("RET: ~p~n", [Ret]),
  exit(Ret).

%% GEN_SERVER CALLBACKS

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  erlang:send_after(100, self(), pop),
  S = dict:new(),
  {ok, S}.

handle_event(_Event, S) -> {ok, S}.

handle_info(pop, S) ->
  erlang:send_after(200, self(), pop),
  {noreply, pop(S)};

handle_info({'DOWN', Ref, process, _, {execute, Order, Group}}, S) ->
  {Order, S2} = clear_ref(Ref, S),
  gen_server:cast(market_broker, {execute, Order, Group}),
  {noreply, S2};

handle_info({'DOWN', Ref, process, _, {cancel, Order, Reason}}, S) ->
  {Order, S2} = clear_ref(Ref, S),
  gen_server:cast(market_broker, {cancel, Order, Reason}),
  {noreply, S2};

handle_info({'DOWN', Ref, process, _, {book, Order}}, S) ->
  {Order, S2} = clear_ref(Ref, S),
  gen_server:cast(market_broker, {book, Order}),
  {noreply, S2};

handle_info({'DOWN', Ref, process, _, normal}, S) ->
  {_, S2} = clear_ref(Ref, S),
  {noreply, S2};

handle_info({'DOWN', Ref, process, _, Reason}, S) ->
  lager:info("DOWN REASON ~p", [Reason]),
  {Order, S2} = clear_ref(Ref, S),
  market_order_queue:push(Order),
  {noreply, S2};

handle_info(Msg, S) -> lager:info("INFO: ~p", [Msg]), {noreply, S}.

handle_call(_Msg, S) -> {reply, ok, S}.

handle_call(_Msg, _, S) -> {reply, ok, S}.

handle_cast(_Msg, S) -> {noreply, S}.

terminate(Reason, _) ->
  lager:info("~p terminating due to ~p", [?MODULE, Reason]),
  ok.

code_change(_, _, S) -> {ok, S}.

pop(S) ->
  case market_order_queue:pop() of
    empty -> S;
    Order ->
      {_, Ref} = spawn_monitor(?MODULE, match_order, [Order]),
      dict:store(Ref, Order, S)
  end.

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

clear_ref(Ref, S) ->
  Order = dict:fetch(Ref, S),
  S2 = dict:erase(Ref, S),
  {Order, S2}.
  
