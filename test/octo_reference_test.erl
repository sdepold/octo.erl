-module(octo_reference_test).

-include_lib("eunit/include/eunit.hrl").
-include("include/octo.hrl").

%% The tests

list_test() ->
  Options          = [{ all_pages }, { per_page, 100 }] ++ request_options(),
  {ok, References} = octo:list_references("sdepold", "octo.erl-test", Options),
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
  ?assertEqual(RefNames, [<<"omnom">>, <<"test">>]).

read_tag_does_not_resolve_branch_names_test() ->
  {err, _} = octo:read_tag("sdepold", "octo.erl-test", "master", request_options()).

read_tag_with_valid_tag_name_test() ->
  {ok, Tag} = octo:read_tag("sdepold", "octo.erl-test", "omnom", request_options()),
  ?assertEqual(Tag#octo_reference.ref, <<"omnom">>).

read_branch_does_not_resolve_tag_names_test() ->
  {err, _} = octo:read_branch("sdepold", "octo.erl-test", "omnom", request_options()).

read_branch_with_valid_branch_name_test() ->
  {ok, Branch} = octo:read_branch("sdepold", "octo.erl-test", "test/head", request_options()),
  ?assertEqual(Branch#octo_reference.ref, <<"test/head">>).

%% Helpers

assertContainsBranches(RefNames) ->
  ?assert(string:str(RefNames, [<<"refs/heads/master">>]) > 0),
  ?assert(string:str(RefNames, [<<"refs/heads/test/head">>]) > 0),
  ?assert(string:str(RefNames, [<<"refs/heads/test/base">>]) > 0).

assertContainsTags(RefNames) ->
  ?assert(string:str(RefNames, [<<"refs/tags/test">>]) > 0).

assertContainsMisc(RefNames) ->
  ?assert(string:str(RefNames, [<<"refs/pull/1/merge">>]) > 0),
  ?assert(string:str(RefNames, [<<"refs/pull/1/head">>]) > 0).

request_options() ->
  AuthToken = os:getenv("AUTH_TOKEN"),
  case AuthToken of
    false -> [];
    _     -> [{auth, pat, AuthToken}]
  end.
