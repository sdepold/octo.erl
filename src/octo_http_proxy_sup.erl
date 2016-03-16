-module(octo_http_proxy_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link(?MODULE, {one_for_one, 5, 60}).

init({RestartStrategy, MaxRestart, MaxTime}) ->
  {ok, {{RestartStrategy, MaxRestart, MaxTime},
       [{octo_http_proxy,
        {octo_http_proxy, start_link, []},
        permanent,
        1000,
        worker,
        [octo_http_proxy]}]}}.
