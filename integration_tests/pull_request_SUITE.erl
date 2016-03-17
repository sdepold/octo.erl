-module(pull_request_SUITE).
-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([create_and_close/1]).
-include("octo.hrl").
-include_lib("common_test/include/ct.hrl").

% Interface

all() -> [create_and_close].

init_per_suite(_Config) ->
  ct:comment("Remember: if the suite fails, maybe it's because the initial "
             "state of the repo was unclean? If so, just make a fresh clone."),

  application:start(octo),

  TestingLogin = os:getenv("TESTING_LOGIN"),
  if TestingLogin =:= false ->
       ct:fail("Please set TESTING_LOGIN environment variable!");
     true -> ok
  end,

  AuthToken = os:getenv("AUTH_TOKEN"),
  if AuthToken =:= false ->
       ct:fail("Please set AUTH_TOKEN environment variable!");
     true -> ok
  end,

  octo:set_credentials(pat, AuthToken),

  Repo = "octo.erl-test",

  Config = [{login, TestingLogin}, {repo, Repo}],

  % making sure there's no stale test PRs
  {ok, PRs} = octo:list_pull_requests(
                TestingLogin,
                Repo),
  case find_test_pull_request_in_list(PRs) of
    null -> ok;
    ExistingPR -> close_pull_request(ExistingPR, Config)
  end,

  Config.

end_per_suite(_Config) ->
  application:stop(octo).

% Tests

create_and_close(Config) ->
  {ok, PR} = octo:create_pull_request(
               ?config(login, Config),
               ?config(repo, Config),
               {
                 {<<"title">>, <<"Test pull request">>},
                 {<<"body">>, <<"Don't touch this. Thanks!">>},
                 {<<"head">>, <<"test/head">>},
                 {<<"base">>, <<"test/base">>}
               }),

  {ok, _Msg} = close_pull_request(PR, Config).

%% Helpers

find_test_pull_request_in_list([]) -> null;
find_test_pull_request_in_list([ PullRequest = #octo_pull_request{ title = <<"Test pull request">> } | _]) -> PullRequest;
find_test_pull_request_in_list([ _ | Rest ]) -> find_test_pull_request_in_list(Rest).

close_pull_request(PullRequest, Config) ->
  octo:update_pull_request(
    ?config(login, Config),
    ?config(repo, Config),
    PullRequest#octo_pull_request.number,
    {{<<"state">>, <<"closed">>}}).
