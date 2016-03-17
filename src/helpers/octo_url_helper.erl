-module(octo_url_helper).
-export([
  generate_url/3,
  pull_request_url/2, pull_request_url/3,
  pull_request_commits_url/3,
  pull_request_files_url/3,
  pull_request_merged_url/3,
  merge_pull_request_url/3,
  reference_url/2, reference_url/3,
  branch_url/2, branch_url/3,
  tag_url/2, tag_url/3,
  options_to_query_params/1
]).

%% Usage: generate_url(pull_request, [Owner, Repo], Options).
generate_url(Thing, Args, Options) ->
  Fun     = list_to_atom(atom_to_list(Thing) ++ "_url"),
  Url     = erlang:apply(octo_url_helper, Fun, Args),
  Query   = options_to_query_params(Options),
  octo_list_helper:join("?", [Url, Query]).

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

options_to_query_params(Options) ->
  Fragments = options_to_query_params(Options, []),
  octo_list_helper:join("&", Fragments).

%% Helper functions

options_to_query_params([], Query) ->
  Query;
options_to_query_params([{ per_page, PerPage }|Rest], Query) ->
  options_to_query_params(Rest, Query ++ ["per_page=" ++ integer_to_list(PerPage)]);
options_to_query_params([_|Rest], Query) ->
  options_to_query_params(Rest, Query).
