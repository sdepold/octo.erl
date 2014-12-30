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
  [PullRequest] = octo:list_pull_requests("sdepold", "octo.erl"),
  ?assertEqual(<<"2014-12-30T20:02:37Z">>, PullRequest#octo_pull_request.updated_at).

-endif.

%% End of Module.
