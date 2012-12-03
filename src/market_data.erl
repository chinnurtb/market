-module(market_data).
-behaviour(gen_server).
-record(state, { procs, hashes, redis }).

-export([start_link/0, init/1]).
-export([handle_event/2, handle_info/2, handle_call/2, handle_call/3, handle_cast/2,
    terminate/2, code_change/3]).

-include("market_data.hrl").

%% PUBLIC API
-export([get_bids/2, get_asks/2]).

get_bids(Book, Symbol) ->
  gen_server:call(?MODULE, {bids, Book, Symbol}).

get_asks(Book, Symbol) ->
  gen_server:call(?MODULE, {asks, Book, Symbol}).

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

handle_call({Side, Book, Symbol}, _, S) ->
  Orders = do_script_raw(orders, [Side, Book, Symbol], [], S),
  {reply, Orders, S};

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

do_script(ScriptName, Keys, Args, S) ->
  case do_script_raw(ScriptName, Keys, Args, S) of
    {error, Reason} -> {error, Reason};
    Reply -> script_reply(Reply)
  end.

do_script_raw(ScriptName, Keys, Args, #state{hashes=Procs, redis=Redis}) ->
  Script = proplists:get_value(ScriptName, Procs),
  case eredis:q(Redis, ["EVALSHA", Script, length(Keys)] ++ Keys ++ Args) of
    {ok, Reply} ->
      lager:info("SCRIPT REPLY: ~p", [Reply]),
      Reply;
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
