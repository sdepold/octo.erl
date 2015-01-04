-module(octo_pull_request_test).

%% octo_pull_request_test: Tests for the module `octo_pull_request`.

-include_lib("eunit/include/eunit.hrl").
-include("include/octo.hrl").

%% The tests

list_pull_requests_test() ->
  {ok, PullRequests} = octo:list_pull_requests("sdepold", "octo.erl", request_options()),
  PullRequest = find_test_pull_request_in_list(PullRequests),
  assert_pull_request(PullRequest).

read_pull_request_test() ->
  {ok, PullRequest} = octo:read_pull_request("sdepold", "octo.erl", 1, request_options()),
  assert_pull_request(PullRequest).

list_pull_request_commits_test() ->
  {ok, [Commit]} = octo:list_pull_request_commits("sdepold", "octo.erl", 1, request_options()),
  assert_commit(Commit).

list_pull_request_files_test() ->
  {ok, [File]} = octo:list_pull_request_files("sdepold", "octo.erl", 1, request_options()),
  assert_file(File).

%% The test helpers

find_test_pull_request_in_list([]) -> null;
find_test_pull_request_in_list([ PullRequest = #octo_pull_request{ title = <<"Test">> } | _]) -> PullRequest;
find_test_pull_request_in_list([ _ | Rest ]) -> find_test_pull_request_in_list(Rest).

assert_pull_request(PullRequest) ->
  ?assertEqual(PullRequest#octo_pull_request.id,         26701040),
  ?assertEqual(PullRequest#octo_pull_request.number,     1),
  ?assertEqual(PullRequest#octo_pull_request.html_url,   <<"https://github.com/sdepold/octo.erl/pull/1">>),
  ?assertEqual(PullRequest#octo_pull_request.title,      <<"Test">>),
  ?assertEqual(PullRequest#octo_pull_request.state,      <<"open">>),
  ?assertEqual(PullRequest#octo_pull_request.body,       <<"This is a test pull request.">>),
  ?assertEqual(PullRequest#octo_pull_request.created_at, <<"2014-12-30T20:02:37Z">>),
  ?assertEqual(PullRequest#octo_pull_request.updated_at, <<"2015-01-03T13:01:19Z">>).

assert_commit(Commit) ->
  ?assertEqual(Commit#octo_commit.html_url, <<"https://github.com/sdepold/octo.erl/commit/b87ca4769260b778c6f4b6e5dadab546f5c89adc">>),
  ?assertEqual(Commit#octo_commit.sha,      <<"b87ca4769260b778c6f4b6e5dadab546f5c89adc">>).

assert_file(File) ->
  ?assertEqual(File#octo_file.sha,       <<"345e6aef713208c8d50cdea23b85e6ad831f0449">>),
  ?assertEqual(File#octo_file.filename,  <<"README.md">>),
  ?assertEqual(File#octo_file.status,    <<"modified">>),
  ?assertEqual(File#octo_file.additions, 1),
  ?assertEqual(File#octo_file.deletions, 12),
  ?assertEqual(File#octo_file.changes,   13),
  ?assertEqual(File#octo_file.blob_url,  <<"https://github.com/sdepold/octo.erl/blob/b87ca4769260b778c6f4b6e5dadab546f5c89adc/README.md">>).

request_options() ->
  AuthToken = os:getenv("AUTH_TOKEN"),
  case AuthToken of
    false -> [];
    _     -> [{auth, pat, AuthToken}]
  end.
