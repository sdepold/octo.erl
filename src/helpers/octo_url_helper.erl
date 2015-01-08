-module(octo_url_helper).
-export([
  pull_request_url/2, pull_request_url/3, pull_request_commits_url/3,
  pull_request_files_url/3, pull_request_merged_url/3
]).
-export([
  reference_url/2, branch_url/2, tag_url/2
]).

repo_url(Owner, Repo) ->
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

reference_url(Owner, Repo) ->
  repo_url(Owner, Repo) ++ "/git/refs".

branch_url(Owner, Repo) ->
  reference_url(Owner, Repo) ++ "/heads".

tag_url(Owner, Repo) ->
  reference_url(Owner, Repo) ++ "/tags".
