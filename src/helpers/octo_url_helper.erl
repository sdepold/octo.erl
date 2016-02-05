-module(octo_url_helper).
-export([
  pull_request_url/2, pull_request_url/3, pull_request_commits_url/3,
  pull_request_files_url/3, pull_request_merged_url/3, merge_pull_request_url/3
]).
-export([
  reference_url/2, reference_url/3, branch_url/2, branch_url/3, tag_url/2, tag_url/3
]).

repo_url(Owner, Repo) when is_list(Owner), is_list(Repo) ->
  "https://api.github.com/repos/" ++ Owner ++ "/" ++ Repo.

pull_request_url(Owner, Repo) ->
  repo_url(Owner, Repo) ++ "/pulls".

pull_request_url(Owner, Repo, Number) ->
  pull_request_url(Owner, Repo) ++ "/" ++ integer_to_list(Number).

pull_request_commits_url(Owner, Repo, Number) ->
  pull_request_url(Owner, Repo, Number) ++ "/commits".

pull_request_files_url(Owner, Repo, Number) ->
  pull_request_url(Owner, Repo, Number) ++ "/files".

pull_request_merged_url(Owner, Repo, Number) ->
  pull_request_url(Owner, Repo, Number) ++ "/merge".

merge_pull_request_url(Owner, Repo, Number) ->
  pull_request_merged_url(Owner, Repo, Number).

reference_url(Owner, Repo) ->
  repo_url(Owner, Repo) ++ "/git/refs".

reference_url(Owner, Repo, RefName) when is_list(RefName) ->
  reference_url(Owner, Repo) ++ "/" ++ RefName.

branch_url(Owner, Repo) ->
  reference_url(Owner, Repo) ++ "/heads".

branch_url(Owner, Repo, BranchName) when is_list(BranchName) ->
  branch_url(Owner, Repo) ++ "/" ++ BranchName.

tag_url(Owner, Repo) ->
  reference_url(Owner, Repo) ++ "/tags".

tag_url(Owner, Repo, TagName) when is_list(TagName) ->
  tag_url(Owner, Repo) ++ "/" ++ TagName.
