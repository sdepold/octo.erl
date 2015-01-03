%% octo_pulls: The handler for pull requests.

-module(octo_pulls).
-include_lib("jsonerl/src/jsonerl.hrl").
-include("octo.hrl").
-export([list/2, read/3]).


%% API

list(Owner, Repo) ->
  Url          = "https://api.github.com/repos/" ++ Owner ++ "/" ++ Repo ++ "/pulls",
  Json         = octo_helper:get(Url),
  PullRequests = jsonerl:decode(Json),
  lists:map(
    fun(PullRequest) ->
      ?struct_to_record(octo_pull_request, PullRequest)
    end,
    PullRequests
  ).

read(Owner, Repo, Number) ->
  Url  = "https://api.github.com/repos/" ++ Owner ++ "/" ++ Repo ++ "/pulls/" ++ integer_to_list(Number),
  Json = octo_helper:get(Url),
  ?json_to_record(octo_pull_request, Json).
