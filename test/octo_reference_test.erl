-module(octo_reference_test).

-include_lib("eunit/include/eunit.hrl").
-include("include/octo.hrl").

%% The tests

list_test_() ->
  {
    timeout, 60, fun () ->
      Options          = [{ all_pages }, { per_page, 100 }] ++ request_options(),
      {ok, References} = octo:list_references("sdepold", "octo.erl-test", Options),
      RefNames         = [ Reference#octo_reference.ref || Reference <- References ],
      assertContainsBranches(RefNames),
      assertContainsTags(RefNames),
      assertContainsMisc(RefNames)
    end
  }.

list_branches_test() ->
  {ok, Branches} = octo:list_branches("sdepold", "octo.erl-test", request_options()),
  RefNames       = [ Branch#octo_reference.ref || Branch <- Branches ],
  ?assertEqual(
    RefNames,
    [<<"master">>, <<"test/base">>, <<"test/head">>]
  ).

list_tags_test() ->
  {ok, Tags} = octo:list_tags("sdepold", "octo.erl-test", request_options()),
  RefNames   = [ Tag#octo_reference.ref || Tag <- Tags ],
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

read_reference_test_() ->
  {
    timeout, 60, fun () ->
      ExpectedBranchRef = "refs/heads/test/head",
      {ok, Branch} = octo:read_reference("sdepold", "octo.erl-test", "heads/test/head", request_options()),
      ?assertEqual(Branch#octo_reference.ref, list_to_binary(ExpectedBranchRef)),
      {ok, FullReferencedBranch} = octo:read_reference("sdepold", "octo.erl-test", ExpectedBranchRef, request_options()),
      ?assertEqual(FullReferencedBranch#octo_reference.ref, list_to_binary(ExpectedBranchRef)),
      {ok, Tag} = octo:read_reference("sdepold", "octo.erl-test", "tags/omnom", request_options()),
      ?assertEqual(Tag#octo_reference.ref, <<"refs/tags/omnom">>),
      {ok, Pull} = octo:read_reference("sdepold", "octo.erl-test", "pull/1/merge", request_options()),
      ?assertEqual(Pull#octo_reference.ref, <<"refs/pull/1/merge">>)
    end
  }.

create_reference_test_() ->
  {
    timeout, 60, fun () ->
      {ok, Reference} = octo:create_reference("sdepold", "octo.erl-test", {
        {<<"ref">>, <<"refs/heads/test/another-head">>},
        {<<"sha">>, <<"f5fab067ab146c389f6661046695fb0bbe1608b0">>}
      }, request_options()),
      {ok, _} = octo:delete_reference("sdepold", "octo.erl-test", "heads/test/another-head", request_options()),
      ?assertEqual(Reference#octo_reference.ref, <<"refs/heads/test/another-head">>)
    end
  }.

create_branch_test_() ->
  {
    timeout, 60, fun () ->
      {ok, Branch} = octo:create_branch(
        "sdepold", "octo.erl-test", "test/another-head",
        "f5fab067ab146c389f6661046695fb0bbe1608b0", request_options()
      ),
      ?assertEqual(Branch#octo_reference.ref, <<"test/another-head">>),
      {ok, Branches} = octo:list_branches("sdepold", "octo.erl-test", request_options()),
      RefNames       = [ Branch#octo_reference.ref || Branch <- Branches ],
      ?assertEqual(
        RefNames,
        [<<"master">>, <<"test/another-head">>, <<"test/base">>, <<"test/head">>]
      ),
      {ok, _} = octo:delete_branch("sdepold", "octo.erl-test", "test/another-head", request_options())
    end
  }.

create_tag_test_() ->
  {
    timeout, 60, fun () ->
      {ok, Tag} = octo:create_tag(
        "sdepold", "octo.erl-test", "another-tag",
        "f5fab067ab146c389f6661046695fb0bbe1608b0", request_options()
      ),
      ?assertEqual(Tag#octo_reference.ref, <<"another-tag">>),
      {ok, Tags} = octo:list_tags("sdepold", "octo.erl-test", request_options()),
      RefNames   = [ Tag#octo_reference.ref || Tag <- Tags ],
      ?assertEqual(RefNames, [<<"another-tag">>, <<"omnom">>, <<"test">>]),
      {ok, _} = octo:delete_tag("sdepold", "octo.erl-test", "another-tag", request_options())
    end
  }.

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
