-module(octo_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/octo.hrl").
-include("test/tests.hrl").

%% Pull Requests

list_pull_requests_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"pull_requests_list.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"pull_requests_list.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(get, _Url, "", <<>>, "") ->
                        {ok, 200, undef, clientref}
                    end),
        meck:expect(hackney, body,
                    fun(clientref) ->
                        {ok, PRJson}
                    end),

        {ok, Results} = octo:list_pull_requests("octocat", "Hello-World"),

        ?assertEqual(Expected, Results),

        ?assert(meck:validate(hackney))
    end]).

read_pull_request_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"pull_request.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"pull_request.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(get, _Url, "", <<>>, "") ->
                        {ok, 200, undef, clientref}
                    end),
        meck:expect(hackney, body,
                    fun(clientref) ->
                        {ok, PRJson}
                    end),

        {ok, Results} = octo:read_pull_request("octocat", "Hello-World", 1347),

        ?assertEqual(Expected, Results),

        ?assert(meck:validate(hackney))
    end]).

list_pull_request_commits_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"pull_request_commits.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"pull_request_commits.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(get, _Url, "", <<>>, "") ->
                        {ok, 200, undef, clientref}
                    end),
        meck:expect(hackney, body,
                    fun(clientref) ->
                        {ok, PRJson}
                    end),

        {ok, Result} = octo:list_pull_request_commits("octocat",
                                                      "Hello-World",
                                                      1347),

        ?assertEqual(Expected, Result),

        ?assert(meck:validate(hackney))
    end]).

list_pull_request_files_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"pull_request_files.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"pull_request_files.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(get, _Url, "", <<>>, "") ->
                        {ok, 200, undef, clientref}
                    end),
        meck:expect(hackney, body,
                    fun(clientref) ->
                        {ok, PRJson}
                    end),

        {ok, Result} = octo:list_pull_request_files("octocat",
                                                    "Hello-World",
                                                    1347),

        ?assertEqual(Expected, Result),

        ?assert(meck:validate(hackney))
    end]).

is_pull_request_merged_test_() ->
  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(get, _Url, "", <<>>, "") ->
                        {ok, Status, undef, clientref}
                    end),

        {ok, Result} = octo:is_pull_request_merged("octocat",
                                                   "Hello-World",
                                                   1347),
        ?assertEqual(Expected, Result),

        ?assert(meck:validate(hackney))
    end
    ||
    {Status, Expected} <- [{204, true}, {404, false}]]).

create_pull_request_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"pull_request_create_response.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"pull_request_create_response.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(post, _Url, "", _Payload, "") ->
                        {ok, 200, undef, clientref}
                    end),
        meck:expect(hackney, body,
                    fun(clientref) ->
                        {ok, PRJson}
                    end),

        {ok, Result} = octo:create_pull_request("octocat",
                                                "Hello-World",
                                                undefined),

        ?assertEqual(Expected, Result),

        ?assert(meck:validate(hackney))
    end]).

update_pull_request_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"pull_request_update_response.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"pull_request_update_response.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(patch, _Url, "", _Payload, "") ->
                        {ok, 200, undef, clientref}
                    end),
        meck:expect(hackney, body,
                    fun(clientref) ->
                        {ok, PRJson}
                    end),

        {ok, Result} = octo:update_pull_request("octocat",
                                                "Hello-World",
                                                1347,
                                                undefined),

        ?assertEqual(Expected, Result),

        ?assert(meck:validate(hackney))
    end]).

merge_pull_request_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"pull_request_merge_response.json"),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(put, _Url, "", <<"{}">>, "") ->
                        {ok, 200, undef, clientref}
                    end),
        meck:expect(hackney, body,
                    fun(clientref) ->
                        {ok, PRJson}
                    end),

        {ok, Result} = octo:merge_pull_request("octocat", "Hello-World", 1347),

        ?assertEqual(PRJson, Result),

        ?assert(meck:validate(hackney))
    end]).

%% References

list_references_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"references.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"references.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(get, _Url, "", _Payload, "") ->
                        {ok, 200, undef, clientref}
                    end),
        meck:expect(hackney, body,
                    fun(clientref) ->
                        {ok, PRJson}
                    end),

        {ok, Result} = octo:list_references("octocat", "Hello-World"),

        ?assertEqual(Expected, Result),

        ?assert(meck:validate(hackney))
    end]).

list_branches_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"branches.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"branches.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(get, _Url, "", _Payload, "") ->
                        {ok, 200, undef, clientref}
                    end),
        meck:expect(hackney, body,
                    fun(clientref) ->
                        {ok, PRJson}
                    end),

        {ok, Result} = octo:list_branches("octocat", "Hello-World"),

        ?assertEqual(Expected, Result),

        ?assert(meck:validate(hackney))
    end]).

list_tags_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"tags.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"tags.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(get, _Url, "", _Payload, "") ->
                        {ok, 200, undef, clientref}
                    end),
        meck:expect(hackney, body,
                    fun(clientref) ->
                        {ok, PRJson}
                    end),

        {ok, Result} = octo:list_tags("octocat", "Hello-World"),

        ?assertEqual(Expected, Result),

        ?assert(meck:validate(hackney))
    end]).

read_reference_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"reference.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"reference.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(get, _Url, "", _Payload, "") ->
                        {ok, StatusCode, undef, clientref}
                    end),
        meck:expect(hackney, body,
                    fun(clientref) ->
                        {ok, PRJson}
                    end),

        {S, Result} = octo:read_reference("octocat",
                                          "Hello-World",
                                          "refs/heads/featureA"),

        if S =:= ok  -> ?assertEqual(Expected, Result);
           S =:= err -> ok
        end,

        ?assert(meck:validate(hackney))
    end
    ||
    StatusCode <- [200, 404]]).

read_tag_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"tag.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"tag.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(get, _Url, "", _Payload, "") ->
                        {ok, 200, undef, clientref}
                    end),
        meck:expect(hackney, body,
                    fun(clientref) ->
                        {ok, PRJson}
                    end),

        {ok, Result} = octo:read_tag("octocat", "Hello-World", "v0.0.1"),

        ?assertEqual(Expected, Result),

        ?assert(meck:validate(hackney))
    end]).

read_branch_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"branch.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"branch.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(get, _Url, "", _Payload, "") ->
                        {ok, 200, undef, clientref}
                    end),
        meck:expect(hackney, body,
                    fun(clientref) ->
                        {ok, PRJson}
                    end),

        {ok, Result} = octo:read_branch("octocat", "Hello-World", "featureA"),

        ?assertEqual(Expected, Result),

        ?assert(meck:validate(hackney))
    end]).

create_reference_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"create_reference_response.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"create_reference_response.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(post, _Url, "", _Payload, "") ->
                        {ok, 200, undef, clientref}
                    end),
        meck:expect(hackney, body,
                    fun(clientref) ->
                        {ok, PRJson}
                    end),

        {ok, Result} = octo:create_reference("octocat",
                                             "Hello-World",
                                             undefined),

        ?assertEqual(Expected, Result),

        ?assert(meck:validate(hackney))
    end]).

create_branch_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"create_branch_response.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"create_branch_response.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(post, _Url, "", _Payload, "") ->
                        {ok, 200, undef, clientref}
                    end),
        meck:expect(hackney, body,
                    fun(clientref) ->
                        {ok, PRJson}
                    end),

        {ok, Result} = octo:create_branch("octocat",
                                          "Hello-World",
                                          "featureA",
                                          "aa218f56b14c9653891f9e74264a383fa43fefbd"),

        ?assertEqual(Expected, Result),

        ?assert(meck:validate(hackney))
    end]).

create_tag_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"create_tag_response.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"create_tag_response.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(post, _Url, "", _Payload, "") ->
                        {ok, 200, undef, clientref}
                    end),
        meck:expect(hackney, body,
                    fun(clientref) ->
                        {ok, PRJson}
                    end),

        {ok, Result} = octo:create_tag("octocat",
                                       "Hello-World",
                                       "v0.0.1",
                                       "aa218f56b14c9653891f9e74264a383fa43fefbd"),

        ?assertEqual(Expected, Result),

        ?assert(meck:validate(hackney))
    end]).

update_reference_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"update_reference_response.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"update_reference_response.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(patch, _Url, "", _Payload, "") ->
                        {ok, 200, undef, clientref}
                    end),
        meck:expect(hackney, body,
                    fun(clientref) ->
                        {ok, PRJson}
                    end),

        {ok, Result} = octo:update_reference("octocat",
                                             "Hello-World",
                                             "refs/heads/featureA",
                                             undefined),

        ?assertEqual(Expected, Result),

        ?assert(meck:validate(hackney))
    end]).

delete_fns_test_() ->
  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(delete, _Url, "", _Payload, "") ->
                        {ok, 200, undef, clientref}
                    end),

        ?assertEqual(
           {ok, null},
           apply(octo, Fun, ["octocat", "Hello-World", Name])),

        ?assert(meck:validate(hackney))
    end
    ||
    {Fun, Name} <- [{delete_reference, "refs/heads/featureA"},
                    {delete_branch, "refs/heads/featureA"},
                    {delete_tag, "refs/tags/v0.0.1"}]]).
