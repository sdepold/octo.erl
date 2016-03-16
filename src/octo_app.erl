-module(octo_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
  hackney:start(),
  octo_cache_sup:start_link(),
  octo_http_proxy_sup:start_link().

stop(_State) ->
  ok.
