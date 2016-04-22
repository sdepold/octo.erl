-module(octo_http_helper_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/octo.hrl").
-include("test/tests.hrl").

ternary_fns_test_() ->
  Url = "http://example.com",
  Body = "{}",

  ?HACKNEY_MOCK([
    fun() ->
      meck:expect(hackney, request,
                  fun(M, U, "", <<>>, [with_body])
                      when M == Method, U =:= Url ->
                      {ok, Status, [], Body}
                  end),

      ?assertEqual(
         Result,
         apply(octo_http_helper, Method, [Url, [], <<>>])
      ),
      ?assert(meck:validate(hackney))
    end
    ||
    Method <- [post, put, patch],
    {Status, Result} <- [{200, {ok, Body}}, {404, {err, #octo_error{}}}]]).

error_passthrough_test_() ->
  Result = {error, whatever},

  TwoArgs = [url, []],
  ThreeArgs = [url, [], <<>>],

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(_Method, _Url, _Headers, _Payload, _Opts) ->
                        Result
                    end),

        ?assertEqual(
           Result,
           apply(octo_http_helper, Method, Args)),
        ?assert(meck:validate(hackney))
    end
    ||
    {Method, Args} <- [{post, ThreeArgs},
                       {put, ThreeArgs},
                       {patch, ThreeArgs},
                       {delete, TwoArgs},
                       {get_response_status_code, TwoArgs}]]).

get_test_() ->
  Url = "http://example.com",
  Body = "{}",

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
                         {304, {ok, cached, Url}},
                         {404, {err, #octo_error{}}}]]).

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
                      {ok, StatusCode, []}
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
  Response = <<"{\"id\": 1}">>,

  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(get, foo, "", <<>>, [with_body]) ->
                        {ok, 304, [], undefined}
                    end),

        octo_cache:store(foo, #octo_cache_entry{result = {ok, whatever}}),

        ?assertEqual(
           {ok, whatever},
           octo_http_helper:read_collection(foo, Options, Id)),
        ?assert(meck:validate(hackney))
    end] ++ [
    fun() ->
        meck:expect(hackney, request,
                    fun(get, url, "", <<>>, [with_body]) ->
                        {ok, Code, [], Response}
                    end),

        ?assertEqual(
           Result,
           octo_http_helper:read_collection(url, Options, Id)),
        ?assert(meck:validate(hackney))
    end
    ||
    {Code, Result} <- [{200, {ok, {{<<"id">>, 1}}}},
                       {404, {err, #octo_error{}}}]]).

pagination_test_() ->
  Id = fun(X) -> X end,

  ?HACKNEY_MOCK([
    fun() ->
        meck:sequence(hackney, request, 5, [{ok, 200, [], <<"[1,2,3]">>}]),

        ?assertEqual(
           {ok, [1,2,3]},
           octo_http_helper:read_collection(url, [all_pages], Id)),
        ?assert(meck:validate(hackney))
    end,
    fun() ->
        meck:sequence(hackney,
                      request,
                      5,
                      [{ok,
                        200,
                        [{<<"Link">>, <<"<whatever>; rel=\"next\"">>}],
                        <<"[1,2,3]">>},
                       {ok,
                        200,
                        [],
                        <<"[4,5,6]">>}]),

        ?assertEqual(
           {ok, [1,2,3,4,5,6]},
           octo_http_helper:read_collection(url, [all_pages], Id)),
        ?assert(meck:validate(hackney))
    end]).
