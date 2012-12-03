-module(market_orders).
-behaviour(gen_server).
-include("market_data.hrl").

-export([start_link/1, init/1]).
-export([handle_event/2, handle_info/2, handle_call/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3]).

%% PUBLIC API
-export([get_bids/1, get_asks/1, book_order/1, cancel_order/1]).

get_bids(Symbol) ->
  gen_server:call(?MODULE, {Symbol, bids}).
get_asks(Symbol) ->
  gen_server:call(?MODULE, {Symbol, asks}).

book_order(Order) ->
  gen_server:call(?MODULE, {book, Order}).

cancel_order(Order) ->
  gen_server:call(?MODULE, {cancel, Order}).

%% GEN_SERVER CALLBACKS

start_link(BookName) ->
  Name = atom_to_list(BookName) ++ "_orders",
  MyName = list_to_atom(Name),
  gen_server:start_link({local, MyName}, ?MODULE, [BookName], []).

init([BookName]) ->
  process_flag(trap_exit, true),
  Book = dict:new(),
  Book2 = dict:store(bids, load_bids(BookName), Book),
  Book3 = dict:store(asks, load_asks(BookName), Book2),
  {ok, Book3}.

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
      market_events:order_placed(Order),
      {reply, {saved, Order}, Book2};
    _ ->
      {reply, Res, Book}
  end;

handle_call({cancel, Order}, _, Book) ->
  cancel_order(Order, Book),
  {reply, ok, Book};

handle_call({Symbol, Type}, _, Book) ->
  SymbolOrders = symbol_orders(Symbol, Type, Book),
  {reply, SymbolOrders, Book};

handle_call(_Msg, _, S) -> {reply, ok, S}.

handle_cast(_Msg, S) -> {noreply, S}.

terminate(Reason, _) ->
  lager:info("~p terminating due to ~p", [?MODULE, Reason]),
  ok.

code_change(_, _, S) -> {ok, S}.

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
    User =:= X#marketOrder.user,
    lager:info("USER ALREADY HAS ~p", [X])
  end, symbol_orders(Symbol, Type, Book)).

save_order(#marketOrder{symbol=Symbol, type=Type} = Order, Book) ->
  lager:info("Saving Order: ~p", [Order]),
  Orders = dict:fetch(plural(Type), Book),
  SymbolOrders = dict:fetch(Symbol, Orders),
  SymbolOrders2 = SymbolOrders ++ [Order],
  Orders2 = dict:store(Symbol, SymbolOrders2, Orders),
  Book2 = dict:store(plural(Type), Orders2, Book),
  market_data:write_order(Order),
  market_events:order_placed(Order),
  {saved, Book2}.

cancel_order(#marketOrder{symbol=Symbol, type=Type} = Order, Book) ->
  lager:info("Cancelling Order: ~p", [Order]),
  Orders = dict:fetch(plural(Type), Book),
  SymbolOrders = dict:fetch(Symbol, Orders),
  SymbolOrders2 = lists:filter(fun(X) ->
    lager:info("~p", [X]),
    X#marketOrder.id =/= Order#marketOrder.id
  end, SymbolOrders),
  Orders2 = dict:store(Symbol, SymbolOrders2, Orders),
  Book2 = dict:store(plural(Type), Orders2, Book),
  market_data:delete_order(Order),
  {saved, Book2}.

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
