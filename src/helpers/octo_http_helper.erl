-module(octo_http_helper).
-export([
  get/2, post/3, patch/3, read_collection/3, get_response_status_code/2
]).

get(Url, Headers) ->
  {ok, _StatusCode, _RespHeaders, ClientRef} = do_request(get, Url, Headers),
  {ok, Body} = hackney:body(ClientRef),
  Body.

post(Url, Headers, Payload) ->
  {ok, _StatusCode, _RespHeaders, ClientRef} = do_request(post, Url, Headers, Payload),
  {ok, Body} = hackney:body(ClientRef),
  Body.

patch(Url, Headers, Payload) ->
  {ok, _StatusCode, _RespHeaders, ClientRef} = do_request(patch, Url, Headers, Payload),
  {ok, Body} = hackney:body(ClientRef),
  Body.

get_response_status_code(Url, Headers) ->
  {ok, StatusCode, _RespHeaders, _ClientRef} = do_request(get, Url, Headers),
  StatusCode.

%% Usage: read_collection(pull_request, [Owner, Repo], Options).
read_collection(Thing, Args, Options) ->
  Fun  = list_to_atom(atom_to_list(Thing) ++ "_url"),
  Url  = erlang:apply(octo_url_helper, Fun, Args),
  Json = get(Url, Options),
  jsonerl:decode(Json).

%% Internals

do_request(Method, Url, Headers) ->
  do_request(Method, Url, Headers, <<>>, []).

do_request(Method, Url, Headers, Payload) ->
  do_request(Method, Url, Headers, Payload, []).

do_request(Method, Url, Headers, Payload, Options) ->
  hackney:start(),
  ParsedHeaders = octo_auth_helper:parse_options(Headers),
  hackney:request(Method, Url, ParsedHeaders, Payload, Options).
