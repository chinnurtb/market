-module(market_pub).
-behaviour(gen_server).
-include("market_data.hrl").

-export([start_link/0, init/1]).
-export([handle_event/2, handle_info/2, handle_call/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3]).

%% GEN_SERVER CALLBACKS

-record(state, {
    socket,
    events
}).

start_link() ->
  gen_server:start_link(?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  {ok, Endpoint} = application:get_env(publish),
  lager:info("ENDPOINT: ~p", [Endpoint]),
  {ok, Context} = erlzmq:context(),
  {ok, Pub} = erlzmq:socket(Context, pub),
  ok = erlzmq:bind(Pub, Endpoint),
  Ref = market_events:subscribe(self()),
  S = #state{socket=Pub, events=Ref},
  {ok, S}.

handle_event(_Event, S) -> {ok, S}.

handle_info({quotes, Quotes}, #state{socket=Pub}=S) ->
  erlzmq:send(Pub, market_pub_proto:quotes(Quotes)),
  {noreply, S};

handle_info({closed, Txn}, #state{socket=Pub}=S) ->
  erlzmq:send(Pub, market_pub_proto:txn(Txn)),
  {noreply, S};

handle_info({order, Order}, #state{socket=Pub}=S) ->
  erlzmq:send(Pub, market_pub_proto:order(Order)),
  {noreply, S};

handle_info(Msg, S) ->
  lager:info("INFO: ~p", [Msg]), 
  {noreply, S}.

handle_call(_Msg, S) -> {reply, ok, S}.

handle_call(_Msg, _, S) -> {reply, ok, S}.

handle_cast(_Msg, S) -> {noreply, S}.

terminate(Reason, #state{socket=Pub, events=Ref}) ->
  lager:info("~p terminating due to ~p", [?MODULE, Reason]),
  market_events:unsubscribe(Ref),
  erlzmq:close(Pub).

code_change(_, _, S) -> {ok, S}.
