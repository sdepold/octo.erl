-module(octo_url_helper_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/octo.hrl").

pull_request_url_test_() ->
  {inparallel,
   [?_assertException(
       error, function_clause,
       octo_url_helper:pull_request_url(haha, "whatever")),
    ?_assertException(
       error, function_clause,
       octo_url_helper:pull_request_url("whoever", haha)),
    ?_assertEqual(
       "https://api.github.com/repos/sdepold/octo.erl/pulls",
       octo_url_helper:pull_request_url("sdepold", "octo.erl")),
    ?_assertEqual(
       "https://api.github.com/repos/Minoru/dotfiles/pulls",
       octo_url_helper:pull_request_url("Minoru", "dotfiles")),
    ?_assertEqual(
       "https://api.github.com/repos/sdepold/octo.erl/pulls/12",
       octo_url_helper:pull_request_url("sdepold", "octo.erl", 12)),
    ?_assertEqual(
       "https://api.github.com/repos/Minoru/dotfiles/pulls/1024",
       octo_url_helper:pull_request_url("Minoru", "dotfiles", 1024)),
    ?_assertException(
       error, badarg,
       octo_url_helper:pull_request_url("Minoru", "dotfiles", "hello")
       =:= "whatever")
   ]}.

pull_request_commits_url_test_() ->
  {inparallel,
   [?_assertEqual(
      "https://api.github.com/repos/sdepold/octo.erl/pulls/12/commits",
      octo_url_helper:pull_request_commits_url("sdepold", "octo.erl", 12)),
    ?_assertEqual(
      "https://api.github.com/repos/Minoru/dotfiles/pulls/1024/commits",
      octo_url_helper:pull_request_commits_url("Minoru", "dotfiles", 1024))
   ]}.

pull_request_files_url_test_() ->
  {inparallel,
   [?_assertEqual(
      "https://api.github.com/repos/sdepold/octo.erl/pulls/12/files",
      octo_url_helper:pull_request_files_url("sdepold", "octo.erl", 12)),
    ?_assertEqual(
      "https://api.github.com/repos/Minoru/dotfiles/pulls/1024/files",
      octo_url_helper:pull_request_files_url("Minoru", "dotfiles", 1024))
   ]}.

pull_request_merged_url_test_() ->
  {inparallel,
   [?_assertEqual(
      "https://api.github.com/repos/sdepold/octo.erl/pulls/12/merge",
      octo_url_helper:pull_request_merged_url("sdepold", "octo.erl", 12)),
    ?_assertEqual(
      "https://api.github.com/repos/Minoru/dotfiles/pulls/1024/merge",
      octo_url_helper:pull_request_merged_url("Minoru", "dotfiles", 1024))
   ]}.

merge_pull_request_url_test_() ->
  {inparallel,
   [?_assertEqual(
      "https://api.github.com/repos/sdepold/octo.erl/pulls/12/merge",
      octo_url_helper:merge_pull_request_url("sdepold", "octo.erl", 12)),
    ?_assertEqual(
      "https://api.github.com/repos/Minoru/dotfiles/pulls/1024/merge",
      octo_url_helper:merge_pull_request_url("Minoru", "dotfiles", 1024))
   ]}.

reference_url_test_() ->
  {inparallel,
   [?_assertEqual(
      "https://api.github.com/repos/sdepold/octo.erl/git/refs",
      octo_url_helper:reference_url("sdepold", "octo.erl")),
    ?_assertEqual(
      "https://api.github.com/repos/Minoru/dotfiles/git/refs",
      octo_url_helper:reference_url("Minoru", "dotfiles")),
    ?_assertEqual(
      "https://api.github.com/repos/sdepold/octo.erl/git/refs/master",
      octo_url_helper:reference_url("sdepold", "octo.erl", "master")),
    ?_assertException(
      error, function_clause,
      octo_url_helper:reference_url("Minoru", "dotfiles", 1024))
   ]}.

branch_url_test_() ->
  {inparallel,
   [?_assertEqual(
      "https://api.github.com/repos/sdepold/octo.erl/git/refs/heads",
      octo_url_helper:branch_url("sdepold", "octo.erl")),
    ?_assertEqual(
      "https://api.github.com/repos/Minoru/dotfiles/git/refs/heads",
      octo_url_helper:branch_url("Minoru", "dotfiles")),
    ?_assertEqual(
      "https://api.github.com/repos/sdepold/octo.erl/git/refs/heads/master",
      octo_url_helper:branch_url("sdepold", "octo.erl", "master")),
    ?_assertException(
      error, function_clause,
      octo_url_helper:branch_url("Minoru", "dotfiles", 1024))
   ]}.

tag_url_test_() ->
  {inparallel,
   [?_assertEqual(
      "https://api.github.com/repos/sdepold/octo.erl/git/refs/tags",
      octo_url_helper:tag_url("sdepold", "octo.erl")),
    ?_assertEqual(
      "https://api.github.com/repos/Minoru/dotfiles/git/refs/tags",
      octo_url_helper:tag_url("Minoru", "dotfiles")),
    ?_assertEqual(
      "https://api.github.com/repos/sdepold/octo.erl/git/refs/tags/r1.0",
      octo_url_helper:tag_url("sdepold", "octo.erl", "r1.0")),
    ?_assertException(
      error, function_clause,
      octo_url_helper:tag_url("Minoru", "dotfiles", 1024))
   ]}.
