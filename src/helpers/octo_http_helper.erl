-module(octo_http_helper).
-export([
  get/1, get/2, read_collection/3, get_response_status_code/2
]).

%% Generic GET request function
get(Url) -> get(Url, []).

get(Url, Headers) ->
  {ok, _StatusCode, _RespHeaders, ClientRef} = do_get_request(Url, Headers),
  {ok, Body} = hackney:body(ClientRef),
  Body.

get_response_status_code(Url, Headers) ->
  {ok, StatusCode, _RespHeaders, _ClientRef} = do_get_request(Url, Headers),
  StatusCode.

%% Usage: read_collection(pull_request, [Owner, Repo], Options).
read_collection(Thing, Args, Options) ->
  Fun  = list_to_atom(atom_to_list(Thing) ++ "_url"),
  Url  = erlang:apply(octo_url_helper, Fun, Args),
  Json = get(Url, Options),
  jsonerl:decode(Json).

%% Internals

do_get_request(Url, Headers) ->
  hackney:start(),
  Headers2 = octo_auth_helper:parse_options(Headers),
  Payload  = <<>>,
  Options  = [],
  hackney:request(get, Url, Headers2, Payload, Options).
