-module(octo_helper).
-export([get/1]).

get(Url) ->
  {ok, StatusCode, RespHeaders, ClientRef} = hackney:request(get, Url, [], <<>>, []),
  {ok, Body} = hackney:body(ClientRef),
  Body.
