-module(pull_request_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([create_and_close/1]).
-include("octo.hrl").
-include_lib("common_test/include/ct.hrl").

% Interface

all() -> [create_and_close].

init_per_suite(_Config) ->
  TestingLogin = os:getenv("TESTING_LOGIN"),
  Repo = "octo.erl-test",

  AuthToken = os:getenv("AUTH_TOKEN"),
  Opts = case AuthToken of
    false -> [];
    _     -> [{auth, pat, AuthToken}]
  end,

  % making sure there's no stale test PRs
  {ok, PRs} = octo_pull_request:list(
                TestingLogin,
                Repo,
                Opts),
  case find_test_pull_request_in_list(PRs) of
    null -> ok;
    ExistingPR -> close_pull_request(ExistingPR, Opts)
  end,

  [{request_options, Opts},
   {login, TestingLogin},
   {repo, Repo}].

end_per_suite(_Config) -> ok.

% Tests

create_and_close(Config) ->
  {ok, PR} = octo_pull_request:create(
               ?config(login, Config),
               ?config(repo, Config),
               {
                 {<<"title">>, <<"Test pull request">>},
                 {<<"body">>, <<"Don't touch this. Thanks!">>},
                 {<<"head">>, <<"test/head">>},
                 {<<"base">>, <<"test/base">>}
               },
               ?config(request_options, Config)),

  {ok, _Msg} = close_pull_request(PR, Config).

%% Helpers

find_test_pull_request_in_list([]) -> null;
find_test_pull_request_in_list([ PullRequest = #octo_pull_request{ title = <<"Test pull request">> } | _]) -> PullRequest;
find_test_pull_request_in_list([ _ | Rest ]) -> find_test_pull_request_in_list(Rest).

close_pull_request(PullRequest, Config) ->
  octo_pull_request:update(
    ?config(login, Config),
    ?config(repo, Config),
    PullRequest#octo_pull_request.number,
    {{<<"state">>, <<"closed">>}},
    ?config(request_options, Config)).
