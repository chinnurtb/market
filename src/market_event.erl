-module(market_event).
-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, code_change/3,
    terminate/2]).

init([Pid]) ->
  {ok, Pid}.

handle_event(Event, Pid) ->
  Pid ! Event,
  {ok, Pid}.

handle_call(_, Pid) -> {reply, ok, Pid}.

handle_info(_, Pid) -> {noreply, Pid}.

code_change(_OldVsn, Pid, _Extra) -> {ok, Pid}.

terminate(_Reason, _Pid) -> ok.
