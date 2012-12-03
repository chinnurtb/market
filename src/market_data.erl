-module(market_data).
-behaviour(gen_server).
-record(state, { procs, hashes, redis }).

-export([start_link/0, init/1]).
-export([handle_event/2, handle_info/2, handle_call/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3]).

-include("market_data.hrl").

%% PUBLIC API
-export([get_bids/2, get_asks/2,
    get_order/1, write_order/1, delete_order/1]).

get_bids(Book, Symbol) ->
  gen_server:call(?MODULE, {bid, Book, Symbol}).

get_asks(Book, Symbol) ->
  gen_server:call(?MODULE, {ask, Book, Symbol}).

get_order(OrderId) ->
  gen_server:call(?MODULE, {order, OrderId}).

write_order(Order) ->
  gen_server:cast(?MODULE, {write, Order}).

delete_order(OrderId) ->
  gen_server:cast(?MODULE, {delete, OrderId}).

%% GEN_SERVER CALLBACKS

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  {ok, Procs} = file:consult("redis.lua.erl"),
  {ok, Redis} = eredis:start_link(),
  Hashes = load_scripts(Procs, Redis),
  {ok, #state{procs=Procs, hashes=Hashes, redis=Redis}}.

handle_event(_Event, S) -> {ok, S}.

handle_info(_Msg, S) -> {noreply, S}.

handle_call(_Msg, S) -> {reply, ok, S}.

%% GET BOOKS
handle_call({Side, Book, Symbol}, _, S) ->
  Orders = do_script(orders, [Side, Book, Symbol], [], S),
  {reply, orders_reply(Orders), S};

handle_call({order, Id}, _, S) ->
  Order = do_script(order, [Id], [], S),
  {reply, order_reply(Order), S};

handle_call(Msg, _, S) ->
  lager:info("handle_call/3 got ~p", [Msg]),
  {reply, ok, S}.

handle_cast({write, Order}, S) ->
  #marketOrder {
    id=Id,
    user=User,
    symbol=Symbol,
    type=Type,
    limit=Limit,
    quantity=Quantity,
    quantity_constraint=QConst,
    time_in_force=Tif,
    timestamp=Ts
  } = Order,
  Market = case Limit of
    none -> market;
    _ -> limit
  end,
  do_script(write_order, [Id, User, Symbol],
    [Type, Limit, Quantity, QConst, Tif, Ts, Market], S),
  {noreply, S};

handle_cast({delete, #marketOrder{id=OrderId}}, S) ->
  do_script(delete_order, [OrderId], [], S),
  {noreply, S};

handle_cast(_Msg, S) -> {noreply, S}.

terminate(Reason, _) ->
  lager:info("~p terminating due to ~p", [?MODULE, Reason]),
  ok.

code_change(_, _, S) -> {ok, S}.

load_scripts(Procs, Redis) ->
  Keys = proplists:get_keys(Procs),
  lists:foldl(fun(ScriptName, Acc) ->
    ScriptFile = proplists:get_value(ScriptName, Procs),
    Script = load_script_file("lua/lib.lua") ++ load_script_file(ScriptFile),
    Reply = load_script(Script, Redis),
    Acc ++ [{ScriptName, Reply}]
  end, [], Keys).

load_script_file(ScriptFile) ->
  {ok, Device} = file:open(ScriptFile, [read]),
  try get_all_lines(Device)
    after file:close(Device)
  end.

get_all_lines(Device) ->
  case io:get_line(Device, "") of 
    eof -> [];
    Line -> Line ++ get_all_lines(Device)
  end.

load_script(Script, Redis) ->
  {ok, Reply} = eredis:q(Redis, ["SCRIPT", "LOAD", Script]),
  Reply.

do_script(ScriptName, Keys, Args, #state{hashes=Procs, redis=Redis}) ->
  Script = proplists:get_value(ScriptName, Procs),
  case eredis:q(Redis, ["EVALSHA", Script, length(Keys)] ++ Keys ++ Args) of
    {ok, Reply} -> Reply;
    {error, Reason} ->
      lager:error(Reason),
      {error, Reason}
  end.

script_reply( [ Atom, Data ] ) -> 
  {list_to_atom(btl(Atom)), tuple_list(Data)}.

orders_reply(L) -> orders_reply(L, []).
orders_reply([], R) -> R;
orders_reply([ Id, User, Symbol, Type, Limit, Quantity, QConst, Tif, Ts | T ], R) ->
  Order = #marketOrder {
    id=val(Id),
    user=val(User),
    symbol=list_to_atom(val(Symbol)),
    type=val(Type),
    limit=val(Limit),
    quantity=val(Quantity),
    quantity_constraint=val(QConst),
    time_in_force=val(Tif),
    timestamp=val(Ts)
  },
  orders_reply(T, R ++ [Order]).

order_reply([]) -> none;
order_reply([undefined]) -> none;
order_reply([undefined | _]) -> none;
order_reply([ Id, User, Symbol, Type, Limit, Quantity, QConst, Tif, Ts ]) ->
  #marketOrder {
    id=val(Id),
    user=val(User),
    symbol=list_to_atom(val(Symbol)),
    type=val(Type),
    limit=val(Limit),
    quantity=val(Quantity),
    quantity_constraint=val(QConst),
    time_in_force=val(Tif),
    timestamp=val(Ts)
  }.

tuple_list(L) -> tuple_list(L, []).
tuple_list([], R) -> R;
tuple_list([ [Atom, Value] | T ], R) ->
  tuple_list(T, [ R | {list_to_atom(btl(Atom)), val(Value)}]).

val(A) ->
  S = btl(A),
  Ret = case string:to_integer(S) of
    {Int, []} -> Int;
    _ -> S
  end,
  case Ret of
    "none" -> none;
    "all" -> all;
    "cancelled" -> cancelled;
    "bid" -> bid;
    "ask" -> ask;
    "day" -> day;
    "immediate" -> immediate;
    "fill" -> fill;
    _ -> Ret
  end.

btl(B) when is_binary(B) -> binary_to_list(B);
btl(B) when is_list(B) -> B;
btl(B) when is_integer(B) -> integer_to_list(B);
btl(B) -> lager:error("WEIRD TYPE ~p", [B]), B.
