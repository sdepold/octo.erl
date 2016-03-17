-module(octo_pagination_helper).
-include("octo.hrl").
-export([get_url/1, read_collection/3]).

get_url({next, Result}) ->
  get_link({next, Result});
get_url({prev, Result}) ->
  get_link({prev, Result});
get_url({first, Result}) ->
  get_link({first, Result});
get_url({last, Result}) ->
  get_link({last, Result}).

read_collection(Arg, Options, ProcessingFun) ->
  case octo_pagination_helper:get_url(Arg) of
    {ok, Url} -> octo_http_helper:read_collection(Url, Options, ProcessingFun);
    Other -> Other
  end.

%% Helper functions

get_link({Type, Result}) ->
  case octo_cache:retrieve({result, Result}) of
    {ok, Value} ->
      Link = Value#octo_cache_entry.headers#octo_cache_headers.link,
      case proplists:get_value(Type, Link) of
        undefined -> {error, no_such_url};
        Url       -> {ok, Url}
      end;
    Other -> Other
  end.
