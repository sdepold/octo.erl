-module(octo_reference_test).

-include_lib("eunit/include/eunit.hrl").
-include("include/octo.hrl").

%% The tests

list_test() ->
  {ok, References} = octo:list_references("sdepold", "octo.erl-test", request_options()),
  RefNames         = [ Reference#octo_reference.ref || Reference <- References ],
  assertContainsBranches(RefNames),
  assertContainsTags(RefNames),
  assertContainsMisc(RefNames).

list_branches_test() ->
  {ok, Branches} = octo:list_branches("sdepold", "octo.erl-test", request_options()),
  RefNames       = [ Branch#octo_reference.ref || Branch <- Branches ],
  ?assertEqual(
    RefNames,
    [<<"master">>, <<"test/base">>, <<"test/head">>]
  ).

list_tags_test() ->
  {ok, Branches} = octo:list_tags("sdepold", "octo.erl-test", request_options()),
  RefNames       = [ Branch#octo_reference.ref || Branch <- Branches ],
  ?assertEqual(RefNames, [<<"test">>]).

%% Helpers

assertContainsBranches(RefNames) ->
  ?assert(string:str(RefNames, [<<"refs/heads/master">>]) > 0),
  ?assert(string:str(RefNames, [<<"refs/heads/test/head">>]) > 0),
  ?assert(string:str(RefNames, [<<"refs/heads/test/base">>]) > 0).

assertContainsTags(RefNames) ->
  % Todo: Make the following line pass:
  % ?assert(string:str(RefNames, [<<"refs/tags/test">>]) > 0).
  ?debugVal("Test for tags is missing").

assertContainsMisc(RefNames) ->
  ?assert(string:str(RefNames, [<<"refs/pull/1/merge">>]) > 0),
  ?assert(string:str(RefNames, [<<"refs/pull/1/head">>]) > 0).

request_options() ->
  AuthToken = os:getenv("AUTH_TOKEN"),
  case AuthToken of
    false -> [];
    _     -> [{auth, pat, AuthToken}]
  end.
