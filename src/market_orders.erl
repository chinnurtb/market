-module(market_orders).
-behaviour(gen_server).
-include("market_data.hrl").

-export([start_link/1, init/1]).
-export([handle_event/2, handle_info/2, handle_call/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3]).

%% GEN_SERVER CALLBACKS

start_link(BookName) ->
  Name = atom_to_list(BookName) ++ "_orders",
  MyName = list_to_atom(Name),
  gen_server:start_link({local, MyName}, ?MODULE, [BookName], []).

init([BookName]) ->
  process_flag(trap_exit, true),
  {ok, load_books(BookName)}.

handle_event(_Event, S) -> {ok, S}.

handle_info(_Msg, S) -> {noreply, S}.

handle_call(_Msg, S) -> {reply, ok, S}.

handle_call({book, Order}, _, Book) ->
  Res = case validate_order(Order, Book) of
    valid -> save_order(Order, Book);
    Invalid -> {error, Invalid}
  end,
  case Res of
    {saved, Book2} ->
      {reply, {saved, Order}, Book2};
    _ ->
      {reply, Res, Book}
  end;


handle_call({exists, #marketOrder{type=Type, symbol=Symbol} = Order}, _, Book) ->
  Orders = dict:fetch(plural(Type), Book),
  SymbolOrders = dict:fetch(Symbol, Orders),
  {reply, lists:member(Order, SymbolOrders), Book};

handle_call({Symbol, Type}, _, Book) ->
  SymbolOrders = symbol_orders(Symbol, Type, Book),
  {reply, SymbolOrders, Book};

handle_call(_Msg, _, S) -> {reply, ok, S}.

handle_cast({cancel, Order, Reason}, Book) ->
  {cancelled, Book2} = cancel_order(Order, Reason, Book),
  {noreply, Book2};

handle_cast({delete, Order}, Book) ->
  {deleted, Book2} = delete_order(Order, Book),
  {noreply, Book2};

handle_cast({close, Order, Txn}, Book) ->
  Diff =  Order#marketOrder.quantity - Txn#marketTxn.quantity,
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
  end,
  {closed, Book2} = close_order(Order, Txn, Book),
  {noreply, Book2};

%handle_cast(flush, Book) ->
%  BookName = atom_to_list(?MODULE)
%  {noreply, Book}

handle_cast(_Msg, S) -> {noreply, S}.

terminate(Reason, _) ->
  lager:info("~p terminating due to ~p", [?MODULE, Reason]),
  ok.

code_change(_, _, S) -> {ok, S}.

load_books(BookName) ->
  Book = dict:new(),
  Book2 = dict:store(bids, load_bids(BookName), Book),
  dict:store(asks, load_asks(BookName), Book2).

load_bids(BookName) ->
  F = fun(X) -> market_data:get_bids(BookName, X) end,
  zip(lists:map(F, ?SYMBOLS), ?SYMBOLS).

load_asks(BookName) ->
  F = fun(X) -> market_data:get_asks(BookName, X) end,
  zip(lists:map(F, ?SYMBOLS), ?SYMBOLS).

symbol_orders(Symbol, Type, Book) ->
  Orders = dict:fetch(plural(Type), Book),
  dict:fetch(Symbol, Orders).

validate_order(#marketOrder { user=User, symbol=Symbol,
    type=Type }, Book) ->
  case user_orders_by_symbol(User, Symbol, Type, Book) of
    [] -> valid;
    _ -> {rejected, "Existing Order"}
  end.

user_orders_by_symbol(User, Symbol, Type, Book) ->
  lists:filter(fun(X) ->
    User == X#marketOrder.user
  end, symbol_orders(Symbol, Type, Book)).

save_order(#marketOrder{symbol=Symbol, type=Type} = Order, Book) ->
  lager:info("Saving Order: ~p", [Order]),
  Orders = dict:fetch(plural(Type), Book),
  SymbolOrders = dict:fetch(Symbol, Orders),
  SymbolOrders2 = lists:sort(fun(A, B) ->
    case A#marketOrder.limit of
      none ->
        %% market orders, sort by time-pref
        A#marketOrder.timestamp =< B#marketOrder.timestamp;
      _ ->
        %% limit orders, sort by price, then time-pref
        case A#marketOrder.limit == B#marketOrder.limit of
          true ->
            A#marketOrder.timestamp =< B#marketOrder.timestamp;
          false ->
            A#marketOrder.limit =< B#marketOrder.limit
        end
    end
  end, SymbolOrders ++ [Order]),
  Orders2 = dict:store(Symbol, SymbolOrders2, Orders),
  Book2 = dict:store(plural(Type), Orders2, Book),
  market_data:write_order(Order),
  market_events:order_placed(Order),
  {saved, Book2}.

delete_order(#marketOrder{symbol=Symbol, type=Type} = Order, Book) ->
  Orders = dict:fetch(plural(Type), Book),
  SymbolOrders = dict:fetch(Symbol, Orders),
  SymbolOrders2 = lists:filter(fun(X) ->
    X#marketOrder.id /= Order#marketOrder.id
  end, SymbolOrders),
  Orders2 = dict:store(Symbol, SymbolOrders2, Orders),
  Book2 = dict:store(plural(Type), Orders2, Book),
  Book2.

cancel_order(Order, Reason, Book) ->
  lager:info("CANCELLING ORDER: ~p FOR ~p", [Order, Reason]),
  Book2 = delete_order(Order, Book),
  market_data:cancel_order(Order, Reason),
  {cancelled, Book2}.

close_order(Order, Txn, Book) ->
  lager:info("CLOSING ~p WITH TXN ~p", [Order, Txn]),
  Book2 = delete_order(Order, Book),
  market_data:close_order(Txn),
  market_events:order_closed(Txn),
  {closed, Book2}.


%% TAKES TWO LISTS
%% A LIST OF ORDER LISTS
%% [ [SymbolOrder...], ... ]
%% AND A LIST OF SYMBOLS
%% AND "ZIPS" THEM TOGETHER INTO A DICT
zip(L, S) -> zip(L, S, dict:new()).
zip(_, [], R) -> R;
zip([], _, R) -> R;
zip([O | OT], [S | ST], R) ->
  zip(OT, ST, dict:store(S, O, R)).

plural(ask) -> asks;
plural(bid) -> bids;
plural(T) -> T.
