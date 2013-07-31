-module(market_data).
-behaviour(gen_server).
-record(state, { procs, hashes, redis }).

-export([start_link/0, init/1]).
-export([handle_event/2, handle_info/2, handle_call/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3]).

-include("market_data.hrl").

%% PUBLIC API
-export([quote/1, quotes/0, execute/4]).

quote(Symbol) ->
  gen_server:call(?MODULE, {quote, Symbol}).

quotes() ->
  Quotes = lists:map(fun(X) ->
    gen_server:call(?MODULE, {quote, X})
  end, ?SYMBOLS),
  Ret = market_utils:zip_proplist(Quotes, ?SYMBOLS),
  Ret.

execute(Order, Contra, Price, Quantity) ->
  gen_server:call(?MODULE, {execute, Order, Contra, Price, Quantity}).

%% GEN_SERVER CALLBACKS

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  {ok, Procs} = file:consult("redis.lua.erl"),
  {ok, Host} = application:get_env(market, redis_host),
  {ok, Port} = application:get_env(market, redis_port),
  {ok, Redis} = eredis:start_link(Host, Port),
  Hashes = load_scripts(Procs, Redis),
  {ok, #state{procs=Procs, hashes=Hashes, redis=Redis}}.

handle_event(_Event, S) -> {ok, S}.

handle_info(_Msg, S) -> {noreply, S}.

handle_call(_Msg, S) -> {reply, ok, S}.

handle_call({quote, Symbol}, _, #state{redis=Redis}=S) ->
  Reply = case eredis:q(Redis, ["HGET", Symbol, "price"]) of
    {ok, undefined} -> 1;
    {ok, Price} -> val(Price);
    {error, Reason} ->
      lager:error("~p", [Reason]),
      {error, Reason}
  end,
  {reply, Reply, S};

handle_call({execute, O, C, P, Q}, _, S) ->
  Keys = case O#marketOrder.type of
    bid -> [ O#marketOrder.user, C#marketOrder.user ];
    ask -> [ C#marketOrder.user, O#marketOrder.user ]
  end,
  Args = [
    Q,
    P
  ],
  Ret = case do_script(execute, Keys, Args, S) of
    [<<"cancelled">>, Reason, BadOrder] ->
      {cancelled, val(Reason), val(BadOrder)};
    [Quantity, Price] ->
      {closed, val(Quantity), val(Price)};
    {error, Error} ->
      lager:error("ERROR EXECUTING ORDERS ~p~n~n~p~n~n", [O, C]),
      {cancelled, val(Error), none}
  end,
  {reply, Ret, S};

handle_call(Msg, _, S) ->
  lager:info("handle_call/3 got ~p", [Msg]),
  {reply, ok, S}.

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
  lager:info("DOING SCRIPT ~p", [ScriptName]),
  Script = proplists:get_value(ScriptName, Procs),
  case eredis:q(Redis, ["EVALSHA", Script, length(Keys)] ++ Keys ++ Args) of
    {ok, Reply} -> Reply;
    {error, Reason} ->
      lager:error("~p", [Reason]),
      {error, Reason}
  end.

val(A) -> market_utils:val(A).
