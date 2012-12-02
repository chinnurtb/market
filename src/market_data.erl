-module(market_data).
-behaviour(gen_server).
-record(state, { procs, hashes }).

-export([start_link/0, init/1]).
-export([handle_event/2, handle_info/2, handle_call/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3]).

-include("market_data.hrl").

%% GEN_SERVER CALLBACKS

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  {ok, Procs} = file:consult("redis.lua.erl"),
  Hashes = load_scripts(Procs),
  {ok, #state{procs=Procs, hashes=Hashes}}.

handle_event(_Event, S) -> {ok, S}.

handle_info(_Msg, S) -> {noreply, S}.

handle_call(_Msg, S) -> {reply, ok, S}.

handle_call({User, #marketOrder{
      type=buy, limit=none, symbol=Symbol,
      quantity=Quantity, time_in_force=Tif,
      quantity_constraint=QConst}}, _, S) ->
  Reply = do_script(market_order, [User, Symbol, buy], [Quantity, Tif, QConst, timestamp()], S),
  lager:info("reply: ~p", [Reply]),
  {reply, Reply, S};

handle_call({User, #marketOrder{
      type=sell, limit=none, symbol=Symbol,
      quantity=Quantity, time_in_force=Tif,
      quantity_constraint=QConst}}, _, S) ->
  Reply = do_script(market_order, [User, Symbol, sell], [Quantity, Tif, QConst, timestamp()], S),
  lager:info("reply: ~p", [Reply]),
  {reply, Reply, S};

handle_call(Msg, _, S) ->
  lager:info("handle_call/3 got ~p", [Msg]),
  {reply, ok, S}.

handle_cast(_Msg, S) -> {noreply, S}.

terminate(Reason, _) ->
  lager:info("~p terminating due to ~p", [?MODULE, Reason]),
  ok.

code_change(_, _, S) -> {ok, S}.

timestamp() ->
  {Mega, Secs, _} = now(),
  Mega*1000000+Secs.

load_scripts(Procs) ->
  Keys = proplists:get_keys(Procs),
  lists:foldl(fun(ScriptName, Acc) ->
    ScriptFile = proplists:get_value(ScriptName, Procs),
    Script = load_script_file("lua/lib.lua") ++ load_script_file(ScriptFile),
    Reply = load_script(Script),
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

load_script(Script) ->
  {ok, Reply} = eredis_pool:q({global, redis}, ["SCRIPT", "LOAD", Script]),
  Reply.

do_script(ScriptName, Keys, Args, #state{hashes=Procs}) ->
  Script = proplists:get_value(ScriptName, Procs),
  case eredis:q({global, redis}, ["EVALSHA", Script, length(Keys)] ++ Keys ++ Args) of
    {ok, Reply} ->
      lager:info("SCRIPT REPLY: ~p", [Reply]),
      script_reply(Reply);
    {error, Reason} ->
      lager:error(Reason),
      {error, Reason}
  end.

script_reply( [ Atom, Data ] ) -> 
  {list_to_atom(btl(Atom)), tuple_list(Data)}.

tuple_list(L) -> tuple_list(L, []).
tuple_list([], R) -> R;
tuple_list([ [Atom, Value] | T ], R) ->
    R2 = R ++ [{list_to_atom(btl(Atom)), val(Value)}],
    tuple_list(T, R2).

val(A) ->
  S = btl(A),
  case string:to_integer(S) of
    {Int, []} -> Int;
    _ -> S
  end.

btl(B) when is_binary(B) -> binary_to_list(B);
btl(B) when is_list(B) -> B;
btl(B) when is_integer(B) -> integer_to_list(B);
btl(B) -> lager:error("WEIRD TYPE ~p", [B]), B.
