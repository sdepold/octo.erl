-module(octo).

%% octo: octo library's entry point.

-export([list_pull_requests/2]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include("include/octo.hrl").
-endif.

%% API

list_pull_requests (User, Repo) -> octo_pulls:list(User, Repo).

%% Tests

-ifdef(TEST).

list_pull_requests_test () ->
  hackney:start(),
  PullRequests = octo:list_pull_requests("sdepold", "octo.erl"),
  PullRequest  = find_test_pull_request_in_list(PullRequests),
  ?assertEqual(26701040, PullRequest#octo_pull_request.id),
  ?assertEqual(1, PullRequest#octo_pull_request.number),
  ?assertEqual(<<"https://github.com/sdepold/octo.erl/pull/1">>, PullRequest#octo_pull_request.html_url),
  ?assertEqual(<<"Test">>, PullRequest#octo_pull_request.title),
  ?assertEqual(<<"open">>, PullRequest#octo_pull_request.state),
  ?assertEqual(<<"This is a test pull request.">>, PullRequest#octo_pull_request.body),
  ?assertEqual(<<"2014-12-30T20:02:37Z">>, PullRequest#octo_pull_request.created_at),
  ?assertEqual(<<"2015-01-03T13:01:19Z">>, PullRequest#octo_pull_request.updated_at).

find_test_pull_request_in_list ([]) -> null;
find_test_pull_request_in_list ([ PullRequest = #octo_pull_request{ title = <<"Test">> } | _]) -> PullRequest;
find_test_pull_request_in_list ([ _ | Rest ]) -> find_test_pull_request_in_list(Rest).

-endif.

%% End of Module.
