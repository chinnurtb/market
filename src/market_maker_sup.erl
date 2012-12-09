-module(market_maker_sup).
-behaviour(supervisor).

-export([init/1, start_link/0]).

-define(MAKER(I), {I, {market_maker, start_link, []}, permanent, 5000, worker, [market_maker]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, { {one_for_one, 10, 60}, [
      ?MAKER(1),
      ?MAKER(2),
      ?MAKER(3),
      ?MAKER(4)
    ]} }.

