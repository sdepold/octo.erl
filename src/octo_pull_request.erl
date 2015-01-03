%% octo_pull_request: The handler for pull requests.

-module(octo_pull_request).
-include_lib("jsonerl/src/jsonerl.hrl").
-include("octo.hrl").
-export([list/2, read/3]).

%% API

list(Owner, Repo) ->
  Url          = "https://api.github.com/repos/" ++ Owner ++ "/" ++ Repo ++ "/pulls",
  Json         = octo_helper:get(Url),
  PullRequests = jsonerl:decode(Json),
  pull_request_blobs_to_record_list(PullRequests).

read(Owner, Repo, Number) ->
  Url  = "https://api.github.com/repos/" ++ Owner ++ "/" ++ Repo ++ "/pulls/" ++ integer_to_list(Number),
  Json = octo_helper:get(Url),
  ?json_to_record(octo_pull_request, Json).

pull_request_blobs_to_record_list(PullRequests) ->
  lists:map(
    fun(PullRequest) ->
      ?struct_to_record(octo_pull_request, PullRequest)
    end,
    PullRequests
  ).
