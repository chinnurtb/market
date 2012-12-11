-module(market_order_queue).
-behaviour(gen_server).
-include("market_data.hrl").

-export([start_link/0, init/1]).
-export([handle_event/2, handle_info/2, handle_call/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3]).

%% PUBLIC API
-export([push/1, pop/0]).

push(Order) ->
  gen_server:call(?MODULE, {push, Order}).
pop() ->
  gen_server:call(?MODULE, pop).

%% GEN_SERVER CALLBACKS

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  Queue = queue:new(),
  {ok, Queue}.

handle_event(_Event, S) -> {ok, S}.

handle_info(_Msg, S) -> {noreply, S}.

handle_call(_Msg, S) -> {reply, ok, S}.

handle_call({push, Order}, _, Queue) ->
  lager:info("queue push ~p", [Order]),
  market_data:write_order(Order),
  Q2 = queue:in(Order, Queue),
  {reply, ok, Q2};

handle_call(pop, _, Queue) ->
  case queue:out(Queue) of
    {{value, Order}, Q2} ->
      lager:info("queue pop ~p", [Order]),
      {reply, Order, Q2};
    {empty, Queue} ->
      {reply, empty, Queue}
  end; 

handle_call(_Msg, _, S) -> {reply, ok, S}.

handle_cast(_Msg, S) -> {noreply, S}.

terminate(Reason, _) ->
  lager:info("~p terminating due to ~p", [?MODULE, Reason]),
  ok.

code_change(_, _, S) -> {ok, S}.
