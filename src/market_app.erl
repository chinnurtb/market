-module(market_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Dispatch = [
    {'_',
      [{'_', market_api_handler, []}]
    }
  ],
  {ok, Port} = application:get_env(market, rpc_port),
  cowboy:start_http(market_api, 100, [{port, Port}],
    [{dispatch, Dispatch}]),
  market_sup:start_link().

stop(_State) ->
  ok.
