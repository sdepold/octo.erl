-module(octo_pull_request).
-include("octo.hrl").
-export([
  list/2, list/3,
  read/4,
  list_commits/2, list_commits/4,
  list_files/2, list_files/4,
  is_merged/4, create/4, update/5, merge/4
]).

%% API

list(Arg, Options) when is_tuple(Arg) and is_list(Options) ->
  octo_pagination_helper:read_collection(Arg,
                                         Options,
                                         fun internal_list_prs/1).
list(Owner, Repo, Options) ->
  Url = octo_url_helper:generate_url(pull_request, [Owner, Repo], Options),
  octo_http_helper:read_collection(Url, Options, fun internal_list_prs/1).

read(Owner, Repo, Number, Options) ->
  Url = octo_url_helper:pull_request_url(Owner, Repo, Number),
  internal_read(Url, Options).

list_commits(Arg, Options) ->
  octo_pagination_helper:read_collection(Arg,
                                         Options,
                                         fun internal_list_commits/1).
list_commits(Owner, Repo, Number, Options) ->
  Url = octo_url_helper:generate_url(
          pull_request_commits, [Owner, Repo, Number], Options),
  octo_http_helper:read_collection(Url, Options, fun internal_list_commits/1).

list_files(Arg, Options) ->
  octo_pagination_helper:read_collection(Arg,
                                         Options,
                                         fun internal_list_files/1).
list_files(Owner, Repo, Number, Options) ->
  Url = octo_url_helper:generate_url(
          pull_request_files, [Owner, Repo, Number], Options),
  octo_http_helper:read_collection(Url, Options, fun internal_list_files/1).

is_merged(Owner, Repo, Number, Options) ->
  Url = octo_url_helper:pull_request_merged_url(Owner, Repo, Number),
  {ok, StatusCode} = octo_http_helper:get_response_status_code(Url, Options),
  Result = case StatusCode of
    404 -> false;
    204 -> true
  end,
  {ok, Result}.

create(Owner, Repo, Payload, Options) ->
  Url          = octo_url_helper:pull_request_url(Owner, Repo),
  PayloadJson  = jsonerl:encode(Payload),
  {ok, Result} = octo_http_helper:post(Url, Options, PayloadJson),
  {ok, ?json_to_record(octo_pull_request, Result)}.

update(Owner, Repo, Number, Payload, Options) ->
  Url          = octo_url_helper:pull_request_url(Owner, Repo, Number),
  PayloadJson  = jsonerl:encode(Payload),
  {ok, Result} = octo_http_helper:patch(Url, Options, PayloadJson),
  {ok, ?json_to_record(octo_pull_request, Result)}.

merge(Owner, Repo, Number, Options) ->
  Url = octo_url_helper:merge_pull_request_url(Owner, Repo, Number),
  octo_http_helper:put(Url, Options, jsonerl:encode({})).

%% Helper functions

internal_read(Url, Options) ->
  case octo_http_helper:get(Url, Options) of
    {ok, cached, CacheKey} ->
      {ok, Entry} = octo_cache:retrieve({url, CacheKey}),
      Entry#octo_cache_entry.result;
    {ok, Json, CacheKey, CacheEntry} ->
      Processed = ?json_to_record(octo_pull_request, Json),
      octo_cache:store(
        CacheKey,
        CacheEntry#octo_cache_entry{result = Processed}),
      {ok, Processed};
    {error, Error} -> {error, Error}
  end.

internal_list_prs(PullRequests) ->
  [ ?struct_to_record(octo_pull_request, PullRequest)
    || (PullRequest) <- PullRequests ].

internal_list_commits(Commits) ->
  [ ?struct_to_record(octo_commit, Commit) || (Commit) <- Commits ].

internal_list_files(Files) ->
  [ ?struct_to_record(octo_file, File) || (File) <- Files ].
