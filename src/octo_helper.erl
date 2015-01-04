-module(octo_helper).
-include("octo.hrl").
-export([get/1, get/2, read_collection/3]).
-export([
  get_pull_request_url/2, get_pull_request_url/3, get_pull_request_commits_url/3,
  get_pull_request_files_url/3
]).
-include_lib("eunit/include/eunit.hrl").

%% Request helpers

%% Helper for reading a collection from Github.
%% Usage: read_collection(pull_request, [Owner, Repo], Options).
read_collection(Thing, Args, Options) ->
  Fun     = list_to_atom("get_" ++ atom_to_list(Thing) ++ "_url"),
  Url     = erlang:apply(octo_helper, Fun, Args),
  Json    = get(Url, Options),
  jsonerl:decode(Json).

%% Generic GET request function
get(Url) -> get(Url, []).

get(Url, Headers) ->
  Headers2 = interprete_auth_options(Headers),
  Payload  = <<>>,
  Options  = [],
  {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(get, Url, Headers2, Payload, Options),
  {ok, Body} = hackney:body(ClientRef),
  Body.

%% URL helpers

get_pull_request_url(Owner, Repo) ->
  "https://api.github.com/repos/" ++ Owner ++ "/" ++ Repo ++ "/pulls".

get_pull_request_url(Owner, Repo, Number) ->
  "https://api.github.com/repos/" ++ Owner ++ "/" ++ Repo ++ "/pulls/" ++ integer_to_list(Number).

get_pull_request_commits_url(Owner, Repo, Number) ->
  "https://api.github.com/repos/" ++ Owner ++ "/" ++ Repo ++ "/pulls/" ++ integer_to_list(Number) ++ "/commits".

get_pull_request_files_url(Owner, Repo, Number) ->
  "https://api.github.com/repos/" ++ Owner ++ "/" ++ Repo ++ "/pulls/" ++ integer_to_list(Number) ++ "/files".

%% Authentication helpers

%% Authentication via Personal API token: https://github.com/blog/1509-personal-api-tokens
interprete_auth_options([]) ->
  [];
interprete_auth_options([{ auth, pat, Pat } | Rest]) ->
  Header = {"Authorization", "Basic " ++ base64:encode_to_string(Pat ++ ":x-oauth-basic")},
  [Header | Rest];
interprete_auth_options([Element|Rest]) ->
  [Element | interprete_auth_options(Rest)].
