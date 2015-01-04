-module(octo_helper).
-include("octo.hrl").
-export([get/1, get/2]).
-export([
  get_pull_request_url/2, get_pull_request_url/3, get_pull_request_commits_url/3
]).

%% Request helpers

get(Url) -> get(Url, []).

%% Authentication via personal access token: https://github.com/blog/1509-personal-api-tokens
get(Url, [{ auth, pat, Pat } | _]) ->
  Headers = [{"Authorization", "Basic " ++ base64:encode_to_string(Pat ++ ":x-oauth-basic")}],
  get(Url, Headers);

get(Url, [_|Rest]) ->
  get(Url, Rest);

get(Url, Headers) ->
  Payload = <<>>,
  Options = [],
  {ok, _StatusCode, _RespHeaders, ClientRef} = hackney:request(get, Url, Headers, Payload, Options),
  {ok, Body} = hackney:body(ClientRef),
  Body.

%% URL helpers

get_pull_request_url(Owner, Repo) ->
  "https://api.github.com/repos/" ++ Owner ++ "/" ++ Repo ++ "/pulls".

get_pull_request_url(Owner, Repo, Number) ->
  "https://api.github.com/repos/" ++ Owner ++ "/" ++ Repo ++ "/pulls/" ++ integer_to_list(Number).

get_pull_request_commits_url(Owner, Repo, Number) ->
  "https://api.github.com/repos/" ++ Owner ++ "/" ++ Repo ++ "/pulls/" ++ integer_to_list(Number) ++ "/commits".
