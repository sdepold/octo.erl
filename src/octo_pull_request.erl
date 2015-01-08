%% octo_pull_request: The handler for pull requests.

-module(octo_pull_request).
-include("octo.hrl").
-export([
  list/3, read/4, list_commits/4, list_files/4, is_merged/4, create/4
]).

%% API

list(Owner, Repo, Options) ->
  PullRequests = octo_http_helper:read_collection(pull_request, [Owner, Repo], Options),
  Result       = [ ?struct_to_record(octo_pull_request, PullRequest) || (PullRequest) <- PullRequests ],
  {ok, Result}.

read(Owner, Repo, Number, Options) ->
  Url    = octo_url_helper:pull_request_url(Owner, Repo, Number),
  Json   = octo_http_helper:get(Url, Options),
  Result = ?json_to_record(octo_pull_request, Json),
  {ok, Result}.


list_commits(Owner, Repo, Number, Options) ->
  Commits = octo_http_helper:read_collection(pull_request_commits, [Owner, Repo, Number], Options),
  Result  = [ ?struct_to_record(octo_commit, Commit) || (Commit) <- Commits ],
  {ok, Result}.

list_files(Owner, Repo, Number, Options) ->
  Files  = octo_http_helper:read_collection(pull_request_files, [Owner, Repo, Number], Options),
  Result = [ ?struct_to_record(octo_file, File) || (File) <- Files ],
  {ok, Result}.

is_merged(Owner, Repo, Number, Options) ->
  Url        = octo_url_helper:pull_request_merged_url(Owner, Repo, Number),
  StatusCode = octo_http_helper:get_response_status_code(Url, Options),
  Result     = case StatusCode of
    404 -> false;
    204 -> true
  end,
  {ok, Result}.

create(Owner, Repo, Payload, Options) ->
  Url         = octo_url_helper:pull_request_url(Owner, Repo),
  PayloadJson = jsonerl:encode(Payload),
  Result      = octo_http_helper:post(Url, Options, PayloadJson),
  {ok, ?json_to_record(octo_pull_request, Result)}.
