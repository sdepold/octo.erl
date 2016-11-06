-module(octo_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/octo.hrl").
-include("test/tests.hrl").

%% General

set_credentials_test_() ->
  Token = "hello",
  % "hello:x-oauth-basic" in base64
  HeaderValue = "Basic aGVsbG86eC1vYXV0aC1iYXNpYw==",
  Header = {"Authorization", HeaderValue},

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(head, _Url, Headers, <<>>, [with_body]) ->
                        ?assertEqual(Header, hd(Headers)),
                        {ok, 404, [], undefined}
                    end),


        octo:set_credentials(pat, Token),
        octo:is_pull_request_merged("octocat", "Hello-World", 1347),

        ?assert(meck:validate(hackney))
    end]).

ratelimit_test_() ->
  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(head, _Url, [], <<>>, [with_body]) ->
                        {ok,
                         404,
                         [{<<"X-RateLimit-Limit">>,     <<"60">>},
                          {<<"X-RateLimit-Remaining">>, <<"29">>},
                          {<<"X-RateLimit-Reset">>,     <<"1458499603">>}],
                         undefined}
                    end),

        octo:is_pull_request_merged("octocat", "Hello-World", 1347),

        ?assertEqual(60, octo:get_ratelimit()),
        ?assertEqual(29, octo:get_ratelimit_remaining()),
        ?assertEqual(1458499603, octo:get_ratelimit_reset()),

        ?assert(meck:validate(hackney))
    end]).

%% Pull Requests

list_pull_requests_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"pull_requests_list.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"pull_requests_list.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(get, _Url, "", <<>>, [with_body]) ->
                        {ok, 200, [], PRJson}
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
                    fun(get, _Url, "", <<>>, [with_body]) ->
                        {ok, 200, [], PRJson}
                    end),

        {ok, Results} = octo:read_pull_request("octocat", "Hello-World", 1347),

        ?assertEqual(Expected, Results),

        ?assert(meck:validate(hackney))
    end,
    fun() ->
        Url = octo_url_helper:pull_request_url("octocat", "Hello-World", 1347),
        octo_cache:store(Url, #octo_cache_entry{result = {ok, result}}),

        meck:expect(hackney, request,
                    fun(get, _Url, [], <<>>, [with_body]) ->
                        {ok, 304, [], <<>>}
                    end),

        ?assertEqual(
          {ok, result},
          octo:read_pull_request("octocat", "Hello-World", 1347)),

        ?assert(meck:validate(hackney))
    end,
    fun() ->
        meck:expect(hackney, request,
                    fun(get, _Url, "", <<>>, [with_body]) ->
                        {error, {closed, <<>>}}
                    end),


        % It'll try to look the result up in the cache and will fail
        ?assertEqual(
           {error, {closed, <<>>}},
           octo:read_pull_request("octocat", "Hello-World", 1347)),
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
                    fun(get, _Url, "", <<>>, [with_body]) ->
                        {ok, 200, [], PRJson}
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
                    fun(get, _Url, "", <<>>, [with_body]) ->
                        {ok, 200, [], PRJson}
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
                    fun(head, _Url, "", <<>>, [with_body]) ->
                        {ok, Status, [], undefined}
                    end),

        {ok, Result} = octo:is_pull_request_merged("octocat",
                                                   "Hello-World",
                                                   1347),
        ?assertEqual(Expected, Result),

        ?assert(meck:validate(hackney))
    end
    ||
    {Status, Expected} <- [{204, true}, {404, false}]]).

error_passthrough_test_() ->
  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(_Method, _Url, "", _Payload, [with_body]) ->
                        {error, whatever}
                    end),

        ?assertEqual(
           {error, whatever},
           apply(octo, Function, Args)),

        ?assert(meck:validate(hackney))
    end
    ||
    {Function, Args} <-
      [ {create_pull_request, ["octocat", "Hello-World", Title, Head, Base]}
        ||
        Title <- ["Test PR", <<"Test PR">>],
        Head <- ["sdepold:test-pr", <<"sdepold:test-pr">>],
        Base <- ["master", <<"master">>]]
      ++
      [{update_pull_request, ["octocat", "Hello-World", 1347, []]},
       {merge_pull_request, ["octocat", "Hello-World", 1347]},
       {create_reference, ["octocat",
                           "Hello-World",
                           "refs/heads/master",
                           "aa218f56b14c9653891f9e74264a383fa43fefbd"]},
       {update_reference, ["octocat",
                           "Hello-World",
                           "refs/heads/featureA",
                           "aa218f56b14c9653891f9e74264a383fa43fefbd"]}]]).

create_pull_request_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"pull_request_create_response.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"pull_request_create_response.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(post, _Url, "", _Payload, [with_body]) ->
                        {ok, 200, [], PRJson}
                    end),

        {ok, Result} = octo:create_pull_request("octocat",
                                                "Hello-World",
                                                Title,
                                                Head,
                                                Base),

        ?assertEqual(Expected, Result),

        ?assert(meck:validate(hackney))
    end
    ||
    Title <- ["Test PR", <<"Test PR">>],
    Head <- ["sdepold:test-pr", <<"sdepold:test-pr">>],
    Base <- ["master", <<"master">>]]).

update_pull_request_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"pull_request_update_response.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"pull_request_update_response.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(patch, _Url, "", _Payload, [with_body]) ->
                        {ok, 200, [], PRJson}
                    end),

        {ok, Result} = octo:update_pull_request("octocat",
                                                "Hello-World",
                                                1347,
                                                []),

        ?assertEqual(Expected, Result),

        ?assert(meck:validate(hackney))
    end]).

merge_pull_request_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"pull_request_merge_response.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"pull_request_merge_response.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(put, _Url, "", <<"{}">>, [with_body]) ->
                        {ok, 200, [], PRJson}
                    end),

        {ok, Result} = octo:merge_pull_request("octocat", "Hello-World", 1347),

        ?assertEqual(Expected, Result),

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
                    fun(get, _Url, "", _Payload, [with_body]) ->
                        {ok, 200, [], PRJson}
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
                    fun(get, _Url, "", _Payload, [with_body]) ->
                        {ok, 200, [], PRJson}
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
                    fun(get, _Url, "", _Payload, [with_body]) ->
                        {ok, 200, [], PRJson}
                    end),

        {ok, Result} = octo:list_tags("octocat", "Hello-World"),

        ?assertEqual(Expected, Result),

        ?assert(meck:validate(hackney))
    end]).

read_reference_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"reference.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"reference.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = {ok, hd(ExpectedL)},

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(get, _Url, "", _Payload, [with_body]) ->
                        {ok, StatusCode, [], PRJson}
                    end),

        Result = octo:read_reference("octocat",
                                     "Hello-World",
                                     "refs/heads/featureA"),

        if StatusCode =:= 200 ->
             ?assertEqual(Expected, Result);
           StatusCode =:= 304 ->
             % It'll try to look the result up in the cache and will fail
             ?assertEqual({error, not_found}, Result);
           StatusCode =:= 404 ->
             ?assertEqual({err, #octo_error{}}, Result)
        end,

        ?assert(meck:validate(hackney))
    end
    ||
    StatusCode <- [200, 304, 404]]).

read_tag_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"tag.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"tag.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(get, _Url, "", _Payload, [with_body]) ->
                        {ok, 200, [], PRJson}
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
                    fun(get, _Url, "", _Payload, [with_body]) ->
                        {ok, 200, [], PRJson}
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
                    fun(post, _Url, "", _Payload, [with_body]) ->
                        {ok, 200, [], PRJson}
                    end),

        {ok, Result} = octo:create_reference("octocat",
                                             "Hello-World",
                                             "refs/heads/featureA",
                                             "aa218f56b14c9653891f9e74264a383fa43fefbd"),

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
                    fun(post, _Url, "", _Payload, [with_body]) ->
                        {ok, 200, [], PRJson}
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
                    fun(post, _Url, "", _Payload, [with_body]) ->
                        {ok, 200, [], PRJson}
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
                    fun(patch, _Url, "", _Payload, [with_body]) ->
                        {ok, 200, [], PRJson}
                    end),

        {ok, Result} = octo:update_reference("octocat",
                                             "Hello-World",
                                             "refs/heads/featureA",
                                             "aa218f56b14c9653891f9e74264a383fa43fefbd"),

        ?assertEqual(Expected, Result),

        ?assert(meck:validate(hackney))
    end]).

delete_fns_test_() ->
  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(delete, _Url, "", _Payload, [with_body]) ->
                        {ok, 200, [], undefined}
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

pagination_test_() ->
  ?HACKNEY_MOCK(lists:concat([
    [fun() ->
         ?assertEqual(
            {error, not_found},
            apply(octo, Fun, [{Atom, nonexistent_result}])),

         ?assert(meck:validate(hackney))
     end,
     fun() ->
         octo_cache:store(url, #octo_cache_entry{result = awesome}),

         ?assertEqual(
            {error, no_such_url},
            apply(octo, Fun, [{Atom, awesome}])),

         ?assert(meck:validate(hackney))
     end,
     fun() ->
         Result = {ok, 404, [],
                   <<"{\"message\":\"uh-oh\", \"documentation_url\":\"http://example.com\"}">>},
         meck:sequence(hackney, request, 5, [Result, Result]),

         octo_cache:store(url, #octo_cache_entry{
                                  headers = #octo_cache_headers{
                                               link = [{next, url},
                                                       {prev, url},
                                                       {first, url},
                                                       {last, url}]},
                                  result = another}),

         ?assertEqual(
            {err, #octo_error{message = <<"uh-oh">>,
                              documentation_url = <<"http://example.com">>}},
            apply(octo, Fun, [{Atom, another}])),
         ?assert(meck:validate(hackney))
     end
    ]
    ||
    Fun <- [list_pull_requests,
            list_pull_request_commits,
            list_pull_request_files,
            list_references,
            list_branches,
            list_tags,
            list_my_organizations,
            list_user_organizations],
    Atom <- [next, prev, first, last]])).

%% Organizations

list_my_organizations_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"my_organizations.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"my_organizations.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(get, _Url, "", <<>>, [with_body]) ->
                        {ok, 200, [], PRJson}
                    end),

        {ok, Results} = octo:list_my_organizations(),

        ?assertEqual(Expected, Results),

        ?assert(meck:validate(hackney))
    end]).

list_user_organizations_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"user_organizations.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"user_organizations.hrl"),
  User = "noname",

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(get, _Url, "", <<>>, [with_body]) ->
                        {ok, 200, [], PRJson}
                    end),

        {ok, Results} = octo:list_user_organizations(User),

        ?assertEqual(Expected, Results),

        ?assert(meck:validate(hackney))
    end]).

read_organization_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"organization.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"organization.hrl"),
  Organization = "noname",

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(get, _Url, "", <<>>, [with_body]) ->
                        {ok, 200, [], PRJson}
                    end),

        {ok, Results} = octo:read_organization(Organization),

        ?assertEqual(Expected, Results),

        ?assert(meck:validate(hackney))
    end]).

update_organization_test_() ->
  {ok, PRJson} = file:read_file(?ASSETS_DIR"update_organization_response.json"),
  {ok, ExpectedL} = file:consult(?ASSETS_DIR"update_organization_response.hrl"),

  ?assertEqual(1, length(ExpectedL)),
  Expected = hd(ExpectedL),

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(patch, _Url, "", _Payload, [with_body]) ->
                        {ok, 200, [], PRJson}
                    end),

        {ok, Result} = octo:update_organization("github", []),

        ?assertEqual(Expected, Result),

        ?assert(meck:validate(hackney))
    end,
    fun() ->
        meck:expect(hackney, request,
                    fun(patch, _Url, "", _Payload, [with_body]) ->
                        {error, whatever}
                    end),

        ?assertEqual(
          {error, whatever},
          octo:update_organization("github", [])),

        ?assert(meck:validate(hackney))
    end]).
