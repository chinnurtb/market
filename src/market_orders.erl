-module(market_orders).
-behaviour(gen_server).
-include("market_data.hrl").

-export([start_link/1, init/1]).
-export([handle_event/2, handle_info/2, handle_call/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3]).

%% PUBLIC API
-export([get_bids/1, get_asks/1, book_order/1]).

get_bids(Symbol) ->
  gen_server:call(?MODULE, {bids, Symbol}).
get_asks(Symbol) ->
  gen_server:call(?MODULE, {asks, Symbol}).

book_order(Order) ->
  gen_server:call(?MODULE, {book, Order}).

%% GEN_SERVER CALLBACKS

start_link(BookName) ->
  Name = atom_to_list(BookName) ++ "_orders",
  MyName = list_to_atom(Name),
  gen_server:start_link({local, MyName}, ?MODULE, [BookName], []).

init([BookName]) ->
  process_flag(trap_exit, true),
  Bids = load_bids(BookName),
  Asks = load_asks(BookName),
  Book = dict:new(),
  dict:append(bids, Bids, Book),
  dict:append(asks, Asks, Book),
  {ok, Book}.

handle_event(_Event, S) -> {ok, S}.

handle_info(_Msg, S) -> {noreply, S}.

handle_call(_Msg, S) -> {reply, ok, S}.

handle_call({book, Order}, _, Book) ->
  {reply, Book, Book};

handle_call({Type, Symbol}, _, Book) ->
  Bids = dict:fetch(Type, Book),
  proplists:lookup_all(Symbol, Bids),
  {reply, Book, Book};

handle_call(_Msg, _, S) -> {reply, ok, S}.

handle_cast(_Msg, S) -> {noreply, S}.

terminate(Reason, _) ->
  lager:info("~p terminating due to ~p", [?MODULE, Reason]),
  ok.

code_change(_, _, S) -> {ok, S}.

load_bids(BookName) ->
  F = fun(X) -> market_data:get_bids(BookName, X) end,
  lists:map(F, ?SYMBOLS).

load_asks(BookName) ->
  F = fun(X) -> market_data:get_asks(BookName, X) end,
  lists:map(F, ?SYMBOLS).
