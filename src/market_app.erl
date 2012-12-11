-module(market_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = [
      {'_', [
          {[<<"api">>, method], market_api, []},
          {'_', market_websocket, []}
      ]}
    ],
    cowboy:start_http(market_websocket, 100,
      [{port, 8080  }],
      [{dispatch, Dispatch}]
    ),
    market_sup:start_link().

stop(_State) ->
    ok.
