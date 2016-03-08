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

  Repo = "octo.erl-test",

  Opts = [{auth, pat, AuthToken}],

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
