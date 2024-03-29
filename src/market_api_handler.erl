-module(market_api_handler).
-include("market_data.hrl").

-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, _Opts) ->
  {ok, Req, undefined_state}.

handle(Req, State) ->
  case cowboy_req:method(Req) of
    {<<"POST">>, _} ->
      {ok, Post, Req2} = cowboy_req:body_qs(Req),
      {ok, R} = cowboy_req:chunked_reply(200, [], Req2),
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
  Res = market_broker:buy(order_params(Params)),
  order_response(Res, Req, State).

do_sell(Params, Req, State) ->
  Res = market_broker:sell(order_params(Params)),
  order_response(Res, Req, State).

do_cancel(Params, Req, State) ->
  OrderId = val(proplists:get_value(<<"id">>, Params)),
  market_broker:cancel(OrderId),
  wait_on_response(Req, State).

order_response({cancelled, R}, Req, State) ->
  cowboy_req:chunk(list_to_binary([<<"{\"result\":\"cancelled\",\"reason\":\"">>, R, <<"\"}\n\n">>]), Req),
  {ok, Req, State};
order_response({error, {_, R}}, Req, State) ->
  cowboy_req:chunk(list_to_binary([<<"{\"result\":\"error\",\"reason\":\"">>, R, <<"\"}\n\n">>]), Req),
  {ok, Req, State};
order_response({closed, _O, Txns}, Req, State) ->
  lists:foreach(fun({_, {P, Q}}) ->
    Msg = list_to_binary(["{\"result\":\"closed\",\"price\":", integer_to_list(P), ",\"quantity\":", integer_to_list(Q), "}\n\n"]),
    cowboy_req:chunk(Msg, Req)
  end, Txns),
  {ok, Req, State};
order_response(#marketOrder{id=Id}, Req, State) ->
  cowboy_req:chunk(list_to_binary([<<"{\"result\":\"booked\",\"id\":\"">>, Id, <<"\"}\n\n">>]), Req),
  {ok, Req, State};
order_response(Response, Req, State) ->
  lager:info("UNKNOWN RESPONSE ~p", [Response]),
  {ok, Req, State}.

order_params(Post) ->
  User = val(proplists:get_value(<<"user">>, Post)),
  Symbol = list_to_atom(val(proplists:get_value(<<"symbol">>, Post))),
  Limit = val(proplists:get_value(<<"limit">>, Post)),
  Quantity = val(proplists:get_value(<<"quantity">>, Post)),
  QConst = val(proplists:get_value(<<"quantity_constraint">>, Post)),
  Tif = val(proplists:get_value(<<"time_in_force">>, Post)),
  {User, Symbol, Limit, Quantity, QConst, Tif}.

wait_on_response(Req, State) ->
  receive
    {booked, #marketOrder{id=Id}} ->
      cowboy_req:chunk(list_to_binary([<<"{\"result\":\"booked\",\"id\":\"">>, Id, <<"\"}\n\n">>]), Req),
      {ok, Req, State};
    shutdown ->
      {ok, Req, State};
    _Msg ->
      wait_on_response(Req, State)
    after 60000 ->
      cowboy_req:chunk(list_to_binary([<<"{\"result\":\"timeout\"}\n\n">>]), Req),
      {ok, Req, State}
    end.


val(A) -> market_utils:val(A).
