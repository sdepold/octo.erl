-module(octo_pull_request_test).

%% octo_pull_request_test: Tests for the module `octo_pull_request`.

-include_lib("eunit/include/eunit.hrl").
-include("include/octo.hrl").

%% The tests

list_pull_requests_test() ->
  PullRequests = octo:list_pull_requests("sdepold", "octo.erl", request_options()),
  PullRequest  = find_test_pull_request_in_list(PullRequests),
  assert_pull_request(PullRequest).

read_pull_request_test() ->
  PullRequest = octo:read_pull_request("sdepold", "octo.erl", 1, request_options()),
  assert_pull_request(PullRequest).

list_pull_request_commits_test() ->
  [Commit] = octo:list_pull_request_commits("sdepold", "octo.erl", 1, request_options()),
  assert_commit(Commit).

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
  ?assertEqual(Commit#octo_commit.sha, <<"b87ca4769260b778c6f4b6e5dadab546f5c89adc">>).

request_options() ->
  AuthToken = os:getenv("AUTH_TOKEN"),
  case AuthToken of
    false -> [];
    _     -> [{ auth, pat, AuthToken }]
  end.
