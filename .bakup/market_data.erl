-module(market_data).
-behaviour(gen_server).
-record(state, { redis, event }).

-export([start_link/0, init/1]).
-export([handle_event/2, handle_info/2, handle_call/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3]).


%% PUBLIC API
-export([
    bid/2, ask/2, bids/1, asks/1,
    bids_above/2, asks_below/2,
    is_valid_ask/1, is_valid_bid/1,
    fill_order/3
]).

ask(From, Symbol) ->
  gen_server:call(?MODULE, {get_ask, From, Symbol}).

asks(Symbol) ->
  gen_server:call(?MODULE, {get_asks, Symbol}).

asks_below(Symbol, Price) ->
  gen_server:call(?MODULE, {get_asks_below, Symbol, Price}).

bid(From, Symbol) ->
  gen_server:call(?MODULE, {get_bid, From, Symbol}).

bids(Symbol) ->
  gen_server:call(?MODULE, {get_bids, Symbol}).

bids_above(Symbol, Price) ->
  gen_server:call(?MODULE, {get_bids_above, Symbol, Price}).

is_valid_ask(Ask) ->
  gen_server:call(?MODULE, {is_valid_ask, Ask}).

is_valid_bid(Bid) ->
  gen_server:call(?MODULE, {is_valid_bid, Bid}).

fill_order(Symbol, Bid, Ask) ->
  gen_server:call(?MODULE, {fill, Symbol, Bid, Ask}).

%% GEN_SERVER CALLBACKS

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  EventRef = market_events:subscribe(self()),
  lager:info("EventRef ~p", [EventRef]),
  {ok, Client} = eredis:start_link(),
  {ok, #state{redis=Client, event=EventRef}}.


handle_event(Event, S) ->
  lager:info("handle_event got ~p", [Event]),
  {ok, S}.

handle_info({bid, {From, Symbol, Price, Amount}}, S) ->
  write_bid(From, Symbol, Price, Amount, S),
  {noreply, S};
  
handle_info({bid_cancel, {From, Symbol}}, S) ->
  delete_bid(From, Symbol, S),
  {noreply, S};

handle_info({ask, {From, Symbol, Price, Amount}}, S) ->
  write_ask(From, Symbol, Price, Amount, S),
  {noreply, S};

handle_info({ask_cancel, {From, Symbol}}, S) ->
  delete_ask(From, Symbol, S),
  {noreply, S};

handle_info({close, {Symbol, Price, Amount}}, S) ->
  write_close(Symbol, Price, Amount, S),
  {noreply, S};

handle_info(Msg, S) ->
  lager:info("handle info got ~p", [Msg]),
  {noreply, S}.

handle_call(Msg, S) ->
  lager:info("handle_call/2 got ~p", [Msg]),
  {reply, ok, S}.

handle_call({get_ask, From, Symbol}, _, S) ->
  Ask = get_ask(From, Symbol, S),
  {reply, Ask, S};

handle_call({get_asks, Symbol}, _, S) ->
  Asks = get_asks(Symbol, S),
  {reply, Asks, S};

handle_call({get_asks_below, Symbol, Price}, _, S) ->
  Asks = get_asks_below(Symbol, Price, S),
  {reply, Asks, S};

handle_call({is_valid_ask, {Asker, Symbol}}, _, S) ->
  Valid = is_valid_ask(Asker, Symbol, S),
  {reply, Valid, S};

handle_call({get_bid, From, Symbol}, _, S) ->
  Bid = get_bid(From, Symbol, S),
  {reply, Bid, S};

handle_call({get_bids, Symbol}, _, S) ->
  Bids = get_bids(Symbol, S),
  {reply, Bids, S};

handle_call({get_bids_above, Symbol, Price}, _, S) ->
  Bids = get_bids_above(Symbol, Price, S),
  {reply, Bids, S};

handle_call({is_valid_bid, {Bidder, Symbol}}, _, S) ->
  Valid = is_valid_bid(Bidder, Symbol, S),
  {reply, Valid, S};

handle_call({fill, Symbol, Bid, Ask}, _, S) ->
  Response = order(Symbol, Bid, Ask, S),
  {reply, Response, S}; 

handle_call(Msg, _, S) ->
  lager:info("handle_call/3 got ~p", [Msg]),
  {reply, ok, S}.

handle_cast(Msg, S) ->
  lager:info("handle cast got ~p", [Msg]),
  {noreply, S}.

terminate(Reason, #state{event=EventRef}) ->
  lager:info("~p terminating due to ~p", [?MODULE, Reason]),
  market_events:unsubscribe(EventRef),
  ok.

code_change(_, _, S) -> {ok, S}.

%% PRIVATE

write_bid(From, Symbol, Price, Amount, #state{redis=Redis}) ->
  BidKey = bid_key(From, Symbol),
  eredis:q(Redis, ["ZADD", "bids:" ++ Symbol, Price, From]),
  eredis:q(Redis, ["HMSET", BidKey, "price", Price, "amount", Amount, "ts", timestamp()]).

delete_bid(From, Symbol, #state{redis=Redis}) ->
  BidKey = bid_key(From, Symbol),
  eredis:q(Redis, ["ZREM", "bids:" ++ Symbol, From]),
  eredis:q(Redis, ["DEL", BidKey]).

get_bid(From, Symbol, #state{redis=Redis}) ->
  {ok, Bid} = eredis:q(Redis, ["HMGET", bid_key(From, Symbol), "price", "amount"]),
  lager:info("get_bid: ~p", [Bid]),
  lbtl(Bid).

is_valid_bid(From, Symbol, #state{redis=Redis}) ->
  case eredis:q(Redis, ["EXISTS", bid_key(From, Symbol)]) of
    {ok, <<"1">>} -> true;
    _ -> false
  end.

get_bids(Symbol, #state{redis=Redis}) ->
  %% Bids is form of [UID, Price, UID, Price...]
  {ok, Bids} = eredis:q(Redis, ["ZRANGE", "bids:" ++ Symbol, 0, -1, "WITHSCORES"]),
  lager:info("get_bids: ~p", [Bids]),
  double_tuple(lbtl(Bids)).

get_bids_above(Symbol, Price, #state{redis=Redis}) ->
  %% Bids is form of [UID, Price, UID, Price...]
  {ok, Bids} = eredis:q(Redis, ["ZRANGEBYSCORE", "bids:" ++ Symbol, Price, "+inf", "WITHSCORES"]),
  lager:info("get_bids_above: ~p", [Bids]),
  Ret = lbtl(Bids),
  double_tuple(Ret).

write_ask(From, Symbol, Price, Amount, #state{redis=Redis}) ->
  AskKey = ask_key(From, Symbol),
  eredis:q(Redis, ["ZADD", "asks:" ++ Symbol, Price, From]),
  eredis:q(Redis, ["HMSET", AskKey, "price", Price, "amount", Amount, "ts", timestamp()]).

delete_ask(From, Symbol, #state{redis=Redis}) ->
  AskKey = ask_key(From, Symbol),
  eredis:q(Redis, ["ZREM", "asks:" ++ Symbol, From]),
  eredis:q(Redis, ["DEL", AskKey]).

get_ask(From, Symbol, #state{redis=Redis}) ->
  {ok, Ask} = eredis:q(Redis, ["HMGET", ask_key(From, Symbol), "price", "amount"]),
  lager:info("get_ask: ~p", [Ask]),
  lbtl(Ask).

is_valid_ask(From, Symbol, #state{redis=Redis}) ->
  lager:info("is_valid_ask called ~p ~p", [From, Symbol]),
  case eredis:q(Redis, ["EXISTS", ask_key(From, Symbol)]) of
    {ok, <<"1">>} -> true;
    _ -> false
  end.

get_asks(Symbol, #state{redis=Redis}) ->
  %% Asks is form of [UID, Price, UID, Price...]
  {ok, Asks} = eredis:q(Redis, ["ZRANGE", "asks:" ++ Symbol, 0, -1, "WITHSCORES"]),
  lager:info("get_asks: ~p", [Asks]),
  double_tuple(lbtl(Asks)).

get_asks_below(Symbol, Price, #state{redis=Redis}) ->
  %% Asks is form of [UID, Price, UID, Price...]
  {ok, Asks} = eredis:q(Redis, ["ZRANGEBYSCORE", "asks:" ++ Symbol, "-inf", Price, "WITHSCORES"]),
  lager:info("get_asks_below: ~p", [Asks]),
  Ret = lbtl(Asks),
  double_tuple(Ret).

order(Symbol, Bid, Ask, #state{redis=Redis}) ->
  {Bidder, _, BidAmount} = Bid,
  {Asker, AskPrice, AskAmount} = Ask,
  BidderKey = user_key(Bidder),
  AskerKey = user_key(Asker),
  Amount = transaction_amount(BidAmount, AskAmount),
  P = [
    ["HINCRBYFLOAT", BidderKey, "cash", -AskPrice],
    ["HINCRBYFLOAT", AskerKey, "cash", AskPrice],
    ["HINCRBYFLOAT", AskerKey, Symbol, -Amount],
    ["HINCRBYFLOAT", BidderKey, Symbol, Amount]
  ],
  lager:info("PIPELINE: ~p", [P]),
  Ret = eredis:qp(Redis, P),
  lager:info("TRANSACTION RESULTS: ~p", [Ret]),
  {closed, Amount, AskPrice}.

write_close(Symbol, Price, Amount, #state{redis=Redis}) ->
  T = timestamp(),
  P = [

    ["HMSET", Symbol, "last_volume", Amount, "last_price", Price],
    ["ZADD", "historical:volume:" ++ Symbol, T, Amount],
    ["ZADD", "historical:price:" ++ Symbol, T, Price]
  ],
  %loger:info("PIPELINE: ~p", [P]),
  Ret = eredis:qp(Redis, P),
  %lager:info("TRANSACTION RESULTS: ~p", [Ret]),
  Ret.

bids_key(Symbol) -> "bids:" ++ Symbol.

bid_key(From, Symbol) ->
  "user:" ++ integer_to_list(From) ++ ":bid:" ++ Symbol.

asks_key(Symbol) -> "asks:" ++ Symbol.

ask_key(From, Symbol) ->
  "user:" ++ integer_to_list(From) ++ ":ask:" ++ Symbol.

user_key(U) ->
  "user:" ++ integer_to_list(U).

timestamp() ->
  {Mega, Secs, _} = now(),
  Mega*1000000+Secs.

transaction_amount(Amount1, Amount2) ->
  case Amount2 > Amount1 of
    true -> Amount1;
    _ -> Amount2
  end.

lbtl(L) -> list_binaries_to_lists(L).
list_binaries_to_lists(L) ->
  F = fun(A, Acc) ->
    S = btl(A),
    Ret = case string:to_integer(S) of
      {Int, []} -> Int;
      _ -> A
    end,
    Acc ++ [Ret]
  end,
  lists:foldl(F, [], L).
btl(B) when is_binary(B) -> binary_to_list(B);
btl(B) when is_list(B) -> B;
btl(B) when is_integer(B) -> integer_to_list(B);
btl(B) -> lager:error("WEIRD TYPE ~p", [B]), B.

double_tuple(List) -> double_tuple(List, []).
double_tuple([], S) -> S;
double_tuple([H1, H2|T], S) -> double_tuple(T, S++[{H1, H2}]).
