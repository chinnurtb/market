-module(market_broker).
-behaviour(gen_server).
-include("market_data.hrl").

-export([start_link/0, init/1]).
-export([handle_event/2, handle_info/2, handle_call/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3]).

%% PUBLIC API
-export([buy/6, sell/6]).

buy(User, Symbol, Limit, Quantity, QConst, Tif) ->
 place_order(#marketOrder {
    id=uuid:to_string(uuid:uuid4()),
    user=User,
    symbol=Symbol,
    type=bid,
    limit=Limit,
    quantity=Quantity,
    quantity_constraint=QConst,
    time_in_force=Tif,
    timestamp=market_utils:timestamp()
  }).

sell(User, Symbol, Limit, Quantity, QConst, Tif) ->
  place_order(#marketOrder {
    id=uuid:to_string(uuid:uuid4()),
    user=User,
    symbol=Symbol,
    type=ask,
    limit=Limit,
    quantity=Quantity,
    quantity_constraint=QConst,
    time_in_force=Tif,
    timestamp=market_utils:timestamp()
  }).

%% GEN_SERVER CALLBACKS

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  {ok, none}.

handle_event(_Event, S) -> {ok, S}.

handle_info(_Msg, S) -> {noreply, S}.

handle_call(_Msg, S) -> {reply, ok, S}.

handle_call(_Msg, _, S) -> {reply, ok, S}.

handle_cast(_Msg, S) -> {noreply, S}.

terminate(Reason, _) ->
  lager:info("~p terminating due to ~p", [?MODULE, Reason]),
  ok.

code_change(_, _, S) -> {ok, S}.

place_order(#marketOrder{limit=Limit} = Order) ->
  case Limit of
    none -> market_orders:book_order(Order);
    _ -> limit_orders:book_order(Order)
  end.
