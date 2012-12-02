-module(market_maker).
-behaviour(gen_server).

-export([start_link/0, init/1]).
-export([handle_event/2, handle_info/2, handle_call/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3]).

-export([find_bid_match/1, find_ask_match/1]).

%% GEN_SERVER CALLBACKS

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  EventRef = market_events:subscribe(self()),
  lager:info("EventRef ~p", [EventRef]),
  {ok, EventRef}.

handle_event(Event, S) ->
  lager:info("got ~p", [Event]),
  {ok, S}.

handle_info({bid, Bid}, S) ->
  spawn(?MODULE, find_bid_match, [Bid]),
  {noreply, S};

handle_info({ask, Ask}, S) ->
  spawn(?MODULE, find_ask_match, [Ask]),
  {noreply, S};
  
handle_info(Msg, S) ->
  lager:info("handle info got ~p", [Msg]),
  {noreply, S}.

handle_call(Msg, S) ->
  lager:info("handle_call/2 got ~p", [Msg]),
  {reply, ok, S}.

handle_call(Msg, _, S) ->
  lager:info("handle_call/3 got ~p", [Msg]),
  {reply, ok, S}.

handle_cast(Msg, S) ->
  lager:info("handle cast got ~p", [Msg]),
  {noreply, S}.

terminate(Reason, EventRef) ->
  lager:info("~p terminating due to ~p", [?MODULE, Reason]),
  market_events:unsubscribe(EventRef),
  ok.

code_change(_, _, S) -> {ok, S}.

%%PRIVATE

find_bid_match({Bidder, Symbol, BidPrice, BidAmount}) ->
  case find_good_asks(Symbol, BidPrice, Bidder) of
    [] ->
      lager:info("no matching or lower asks found");
    Asks ->
      lager:info("found an good asks ~p", [Asks])
      %Result = fill_match(Symbol,
      %  {Bidder, BidPrice, BidAmount},
      %  {Asker, AskPrice, AskAmount}
      %),
      %lager:info("RESULT: ~p", [Result]),
      %{_, ClosedAmount, _} = Result,
      %cancel_or_reask(Symbol, Asker, AskAmount, ClosedAmount, AskPrice),
      %cancel_or_rebid(Symbol, Bidder, BidAmount, ClosedAmount, BidPrice);
  end.

find_good_asks(Symbol, BidPrice, Bidder) ->
  Asks = market_data:asks_below(Symbol, BidPrice),
  Asks2 = filter_own_asks(Asks, Bidder),
  filter_asks(Asks, Symbol).

filter_own_asks(Asks, Bidder) ->
  lists:filter(fun(X) ->
    {Asker, _} = X,
    Asker /= Bidder
  end, Asks).

filter_asks(Asks, Symbol) ->
  lists:filter(fun(Ask) -> {Asker, _} = Ask, market_data:is_valid_ask({Asker, Symbol}) end, Asks).

find_ask_match({Asker, Symbol, AskPrice, AskAmount}) ->
  case find_good_bids(Symbol, AskPrice, Asker) of
    [] ->
      lager:info("no matching or higher bids found");
    Bids ->
      lager:info("found a good bids ~p", [Bids])
      %Result = fill_match(Symbol,
      %  {Bidder, BidPrice, BidAmount},
      %  {Asker, AskPrice, AskAmount}
      %),
      %lager:info("RESULT: ~p", [Result]),
      %{_, ClosedAmount, _} = Result,
      %cancel_or_reask(Symbol, Asker, AskAmount, ClosedAmount, AskPrice),
      %cancel_or_rebid(Symbol, Bidder, BidAmount, ClosedAmount, BidPrice);
  end.

find_good_bids(Symbol, AskPrice, Asker) ->
  Bids = market_data:bids_above(Symbol, AskPrice),
  Bids2 = filter_own_bids(Bids, Asker),
  filter_bids(Bids).

filter_own_bids(Bids, Asker) ->
  lists:filter(fun(X) ->
    {Bidder, _} = X,
    Asker /= Bidder
  end, Bids).

filter_bids(Bids) ->
  lists:filter(fun(Bid) -> market_data:is_valid_bid(Bid) end, Bids).

cancel_or_reask(Symbol, Asker, AskAmount, ClosedAmount, AskPrice) ->
  case ClosedAmount < AskAmount of
    true ->
      lager:info("reasking"),
      market_events:ask(Asker, Symbol, AskAmount-ClosedAmount, AskPrice);
    _ ->
      lager:info("fullfilled ask"),
      market_events:cancel_ask(Asker, Symbol)
  end.

cancel_or_rebid(Symbol, Bidder, BidAmount, ClosedAmount, BidPrice) ->
  case ClosedAmount < BidAmount of
    true ->
      lager:info("rebidding ~p ~p", [BidAmount, ClosedAmount]),
      market_events:bid(Bidder, Symbol, BidAmount-ClosedAmount, BidPrice);
    _ ->
      lager:info("fullfilled bid"),
      market_events:cancel_bid(Bidder, Symbol)
  end.

fill_match(Symbol, Bid, Ask) ->
  market_broker:fill_match(Symbol, Bid, Ask).
