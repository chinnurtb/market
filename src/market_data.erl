-module(market_data).
-behaviour(gen_server).
-record(state, { procs, hashes, redis }).

-export([start_link/0, init/1]).
-export([handle_event/2, handle_info/2, handle_call/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3]).

-include("market_data.hrl").

%% PUBLIC API
-export([get_bids/2, get_asks/2,
    get_order/1, write_order/1, cancel_order/2, close_order/2]).

-export([quote/1, execute/5]).

get_bids(Book, Symbol) ->
  gen_server:call(?MODULE, {bid, Book, Symbol}).

get_asks(Book, Symbol) ->
  gen_server:call(?MODULE, {ask, Book, Symbol}).

get_order(OrderId) ->
  gen_server:call(?MODULE, {order, OrderId}).

write_order(Order) ->
  gen_server:cast(?MODULE, {write, Order}).

cancel_order(OrderId, Reason) ->
  gen_server:cast(?MODULE, {cancel, OrderId, Reason}).

close_order(Order, Txn) ->
  gen_server:cast(?MODULE, {close, Order, Txn}).

quote(Symbol) ->
  gen_server:call(?MODULE, {quote, Symbol}).

execute(Lock, Order, Contra, Price, Quantity) ->
  gen_server:call(?MODULE, {execute, Lock, Order, Contra, Price, Quantity}).


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

handle_call({quote, Symbol}, _, #state{redis=Redis}=S) ->
  Reply = case eredis:q(Redis, ["HGET", Symbol, "price"]) of
    {ok, undefined} -> 1;
    {ok, Price} -> val(Price);
    {error, Reason} ->
      lager:error("~p", [Reason]),
      {error, Reason}
  end,
  {reply, Reply, S};

handle_call({execute, L, O, C, P, Q}, _, S) ->
  Keys = [
    O#marketOrder.id,
    C#marketOrder.id
  ],
  Args = [
    O#marketOrder.limit,
    O#marketOrder.quantity,
    O#marketOrder.quantity_constraint,
    O#marketOrder.time_in_force,
    O#marketOrder.timestamp,
    O#marketOrder.user,
    O#marketOrder.symbol,
    O#marketOrder.type,
  
    C#marketOrder.limit,
    C#marketOrder.quantity,
    C#marketOrder.quantity_constraint,
    C#marketOrder.time_in_force,
    C#marketOrder.timestamp,
    C#marketOrder.user,
    C#marketOrder.symbol,
    C#marketOrder.type,

    Q,
    P,
    L
  ],
  Ret = case do_script(execute, Keys, Args, S) of
    [<<"cancelled">>, Reason, BadOrder] ->
      {cancelled, val(Reason), val(BadOrder)};
    [Tx, Quantity, Price] ->
      {closed, #marketTxn {
          id=val(Tx),
          quantity=val(Quantity),
          price=val(Price),
          buy=O#marketOrder.id,
          sell=C#marketOrder.id
        }
      }
  end,
  {reply, Ret, S};

handle_call(Msg, _, S) ->
  lager:info("handle_call/3 got ~p", [Msg]),
  {reply, ok, S}.

handle_cast({write, Order}, S) ->
  do_write_order(Order, S),
  {noreply, S};

handle_cast({cancel, #marketOrder{id=Order}, Reason}, S) ->
  do_script(cancel, [Order], [Reason], S),
  {noreply, S};

handle_cast({close, Order, Txn}, S) ->
  do_write_order(Order, S),
  TxId = Txn#marketTxn.id,
  do_script(close, [TxId], [], S),
  {noreply, S};
  
handle_cast(_Msg, S) -> {noreply, S}.

terminate(Reason, _) ->
  lager:info("~p terminating due to ~p", [?MODULE, Reason]),
  ok.

code_change(_, _, S) -> {ok, S}.

do_write_order(Order, S) ->
  #marketOrder {
    id=Id,
    user=User,
    symbol=Symbol,
    type=Type,
    limit=Limit,
    quantity=Quantity,
    quantity_constraint=QConst,
    time_in_force=Tif,
    timestamp=Ts,
    state=OState
  } = Order,
  Market = case Limit of
    none -> market;
    _ -> limit
  end,
  do_script(write, [Id, User, Symbol],
    [Type, Limit, Quantity, QConst, Tif, Ts, OState, Market], S).

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
      lager:error("~p", [Reason]),
      {error, Reason}
  end.

orders_reply(L) -> orders_reply(L, []).
orders_reply([], R) -> R;
orders_reply([ Id, User, Symbol, Type, Limit, Quantity, QConst, Tif, Ts, State | T ], R) ->
  Order = #marketOrder {
    id=val(Id),
    user=val(User),
    symbol=list_to_atom(val(Symbol)),
    type=val(Type),
    limit=val(Limit),
    quantity=val(Quantity),
    quantity_constraint=val(QConst),
    time_in_force=val(Tif),
    timestamp=val(Ts),
    state=val(State)
  },
  orders_reply(T, R ++ [Order]).

order_reply([]) -> none;
order_reply([undefined]) -> none;
order_reply([undefined | _]) -> none;
order_reply([ Id, User, Symbol, Type, Limit, Quantity, QConst, Tif, Ts, State ]) ->
  #marketOrder {
    id=val(Id),
    user=val(User),
    symbol=list_to_atom(val(Symbol)),
    type=val(Type),
    limit=val(Limit),
    quantity=val(Quantity),
    quantity_constraint=val(QConst),
    time_in_force=val(Tif),
    timestamp=val(Ts),
    state=val(State)
  }.

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
    "closed" -> closed;
    "new" -> new;
    "booked" -> booked;
    "locked" -> locked;
    _ -> Ret
  end.

btl(B) when is_binary(B) -> binary_to_list(B);
btl(B) when is_list(B) -> B;
btl(B) when is_integer(B) -> integer_to_list(B);
btl(B) -> lager:error("WEIRD TYPE ~p", [B]), B.
