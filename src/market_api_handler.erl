-module(market_api_handler).
-include("market_data.hrl").

-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, _Opts) ->
  {ok, Req, undefined_state}.

handle(Req, State) ->
  case cowboy_req:method(Req) of
    {<<"POST">>, _} ->
      {ok, Post, Req2} = cowboy_req:body_qs(Req),
      R = cowboy_req:chunked_reply(200, [], Req2),
      case get_method(Post) of
        <<"buy">> -> do_buy(Post, R, State);
        <<"sell">> -> do_sell(Post, R, State);
        <<"cancel">> -> do_cancel(Post, R, State)
      end;
    _ ->
      cowboy_req:reply(405, [], <<"NOT ALLOWED\n\n">>, Req),
      {ok, Req, State}
  end.

terminate(_Req, _State) -> ok.


get_method(Post) ->
  proplists:get_value(<<"method">>, Post).
  
do_buy(Params, Req, State) ->
  {queued, _OrderId} = market_broker:buy(order_params(Params)),
  wait_on_response(Req, State).

do_sell(Params, Req, State) ->
  {queued, _OrderId} = market_broker:sell(order_params(Params)),
  wait_on_response(Req, State).

do_cancel(Params, Req, State) -> ok.

order_params(Post) ->
  User = val(proplists:get_value(<<"user">>, Post)),
  Symbol = list_to_atom(val(proplists:get_value(<<"symbol">>, Post))),
  Limit = val(proplists:get_value(<<"limit">>, Post)),
  Quantity = val(proplists:get_value(<<"quantity">>, Post)),
  QConst = val(proplists:get_value(<<"qconst">>, Post)),
  Tif = val(proplists:get_value(<<"tif">>, Post)),
  {User, Symbol, Limit, Quantity, QConst, Tif}.

wait_on_response(Req, State) ->
  receive
    {cancelled, _O, R} ->
      cowboy_req:chunk([<<"cancelled:">>, R], Req),
      {ok, Req, State};
    {error, _O, R} ->
      cowboy_req:chunk([<<"error:">>, R], Req),
      {ok, Req, State};
    {booked, #marketOrder{id=Id}} ->
      cowboy_req:chunk([<<"booked:">>, Id], Req),
      {ok, Req, State};
    {closed, O, Txns} ->
      lists:foreach(fun({_, X}) ->
        #marketTxn{price=P, quantity=Q} = X,
        Msg = [<<"closed:">>, P, <<":">>, Q],
        lager:info("RESPONSE: ~p", [Msg]),
        cowboy_req:chunk(Msg, Req)
      end, Txns),
      {ok, Req, State};
    shutdown ->
      {ok, Req, State};
    Msg ->
      lager:info("UNKNOWN MESSAGE ~p", [Msg]),
      wait_on_response(Req, State)
    after 60000 ->
        {ok, Req, State}
    end.


val(A) -> market_utils:val(A).
