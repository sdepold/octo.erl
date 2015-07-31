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
    [<<"master">>, <<"test/base">>, <<"test/head">>],
    RefNames
  ).

list_tags_test() ->
  {ok, Tags} = octo:list_tags("sdepold", "octo.erl-test", request_options()),
  RefNames   = [ Tag#octo_reference.ref || Tag <- Tags ],
  ?assertEqual([<<"omnom">>, <<"test">>], RefNames).

read_tag_does_not_resolve_branch_names_test() ->
  {err, _} = octo:read_tag("sdepold", "octo.erl-test", "master", request_options()).

read_tag_with_valid_tag_name_test() ->
  {ok, Tag} = octo:read_tag("sdepold", "octo.erl-test", "omnom", request_options()),
  ?assertEqual(<<"omnom">>, Tag#octo_reference.ref).

read_branch_does_not_resolve_tag_names_test() ->
  {err, _} = octo:read_branch("sdepold", "octo.erl-test", "omnom", request_options()).

read_branch_with_valid_branch_name_test() ->
  {ok, Branch} = octo:read_branch("sdepold", "octo.erl-test", "test/head", request_options()),
  ?assertEqual(<<"test/head">>, Branch#octo_reference.ref).

read_reference_test_() ->
  {
    timeout, 60, fun () ->
      ExpectedBranchRef = "refs/heads/test/head",
      {ok, Branch} = octo:read_reference("sdepold", "octo.erl-test", "heads/test/head", request_options()),
      ?assertEqual(list_to_binary(ExpectedBranchRef), Branch#octo_reference.ref),
      {ok, FullReferencedBranch} = octo:read_reference("sdepold", "octo.erl-test", ExpectedBranchRef, request_options()),
      ?assertEqual(list_to_binary(ExpectedBranchRef), FullReferencedBranch#octo_reference.ref),
      {ok, Tag} = octo:read_reference("sdepold", "octo.erl-test", "tags/omnom", request_options()),
      ?assertEqual(<<"refs/tags/omnom">>, Tag#octo_reference.ref),
      {ok, Pull} = octo:read_reference("sdepold", "octo.erl-test", "pull/1/merge", request_options()),
      ?assertEqual(<<"refs/pull/1/merge">>, Pull#octo_reference.ref)
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
      ?assertEqual(<<"refs/heads/test/another-head">>, Reference#octo_reference.ref)
    end
  }.

create_branch_test_() ->
  {
    timeout, 60, fun () ->
      {ok, Branch} = octo:create_branch(
        "sdepold", "octo.erl-test", "test/another-head",
        "f5fab067ab146c389f6661046695fb0bbe1608b0", request_options()
      ),
      ?assertEqual(<<"test/another-head">>, Branch#octo_reference.ref),
      {ok, Branches} = octo:list_branches("sdepold", "octo.erl-test", request_options()),
      RefNames       = [ Branch#octo_reference.ref || Branch <- Branches ],
      ?assertEqual(
        [<<"master">>, <<"test/another-head">>, <<"test/base">>, <<"test/head">>],
        RefNames
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
      ?assertEqual(<<"another-tag">>, Tag#octo_reference.ref),
      {ok, Tags} = octo:list_tags("sdepold", "octo.erl-test", request_options()),
      RefNames   = [ Tag#octo_reference.ref || Tag <- Tags ],
      ?assertEqual([<<"another-tag">>, <<"omnom">>, <<"test">>], RefNames),
      {ok, _} = octo:delete_tag("sdepold", "octo.erl-test", "another-tag", request_options())
    end
  }.

update_reference_test_() ->
  {
    timeout, 60, fun () ->
      {ok, MasterBranch}                = octo:read_branch("sdepold", "octo.erl-test", "master", request_options()),
      {ok, TestHeadBranch}              = octo:read_branch("sdepold", "octo.erl-test", "test/head", request_options()),
      {{<<"sha">>, MasterSha}, _, _}    = MasterBranch#octo_reference.object,
      {{<<"sha">>, TestHeadSha}, _, _}  = TestHeadBranch#octo_reference.object,
      {ok, NewMasterBranch}             = octo:create_branch("sdepold", "octo.erl-test", "test/master", binary_to_list(MasterSha), request_options()),
      {{<<"sha">>, NewMasterSha}, _, _} = NewMasterBranch#octo_reference.object,

      ?assertEqual(<<"test/master">>, NewMasterBranch#octo_reference.ref),
      ?assertEqual(MasterSha, NewMasterSha),

      Payload                                  = {{<<"sha">>, TestHeadSha}, {<<"force">>, true}},
      {ok, UpdatedNewMasterBranch}             = octo:update_reference("sdepold", "octo.erl-test", "refs/heads/test/master", Payload, request_options()),
      {{<<"sha">>, UpdatedNewMasterSha}, _, _} = UpdatedNewMasterBranch#octo_reference.object,

      ?assertEqual(TestHeadSha, UpdatedNewMasterSha),

      {ok, _} = octo:delete_branch("sdepold", "octo.erl-test", "test/master", request_options())
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
