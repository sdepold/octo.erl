-module(octo_http_helper_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/octo.hrl").
-include("test/tests.hrl").

ternary_fns_test_() ->
  Url = "http://example.com",
  Body = "empty!",

  ?HACKNEY_MOCK([
    fun() ->
      meck:expect(hackney, request,
                  fun(M, U, "", <<>>, [with_body])
                      when M == Method, U =:= Url ->
                      {ok, Status, [], Body}
                  end),

      ?assertEqual(
         {StatusTerm, Body},
         apply(octo_http_helper, Method, [Url, [], <<>>])
      ),
      ?assert(meck:validate(hackney))
    end
    ||
    Method <- [post, put, patch],
    {Status, StatusTerm} <- [{200, ok}, {404, err}]]).

get_test_() ->
  Url = "http://example.com",
  Body = "empty!",

  ?HACKNEY_MOCK([
    fun() ->
      meck:expect(hackney, request,
                  fun(get, U, "", <<>>, [with_body]) when U =:= Url ->
                      {ok, Status, [], Body}
                  end),

      ?assertEqual(
         Result,
         octo_http_helper:get(Url, [])
      ),
      ?assert(meck:validate(hackney))
    end
    ||
    {Status, Result} <- [{200, {ok, Body, Url, #octo_cache_entry{}}},
                         {404, {err, Body}}]]).

delete_test_() ->
  Url = "http://example.com",

  ?HACKNEY_MOCK([
    fun() ->
      meck:expect(hackney, request,
                  fun(delete, U, "", <<>>, [with_body]) when U =:= Url ->
                      {ok, Status, [], undef}
                  end),

      ?assertEqual(
         {StatusTerm, null},
         octo_http_helper:delete(Url, [])
      ),
      ?assert(meck:validate(hackney))
    end
    ||
    {Status, StatusTerm} <- [{200, ok}, {404, err}]]).

get_response_status_code_test_() ->
  Url = "http://example.com",

  ?HACKNEY_MOCK([
    fun() ->
      meck:expect(hackney, request,
                  fun(head, U, "", <<>>, [with_body]) when U =:= Url ->
                      {ok, StatusCode, [], undef}
                  end),

      ?assertEqual(
         {ok, StatusCode},
         octo_http_helper:get_response_status_code(Url, [])
      ),
      ?assert(meck:validate(hackney))
    end
    ||
    StatusCode <- [200, 404]]).

read_collection_test_() ->
  Options = [],
  Id = fun(X) -> X end,

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(get, url, "", <<>>, [with_body]) ->
                        {ok, 200, [], <<"{\"id\": 1}">>}
                    end),

        ?assertEqual(
           {ok, {{<<"id">>, 1}}},
           octo_http_helper:read_collection(url, Options, Id)),
        ?assert(meck:validate(hackney))
    end]).
