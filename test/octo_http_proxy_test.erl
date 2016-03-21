-module(octo_http_proxy_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/octo.hrl").
-include("test/tests.hrl").

cache_headers_test_() ->
  ETag = "aabbccdd-123",
  LastModified = "Sat, 24 Jan 2015 23:27:58 GMT",
  Headers = [{<<"ETag">>, ETag}, {<<"Last-Modified">>, LastModified}],
  Body = <<"[]">>,

  ?HACKNEY_MOCK([
    fun() ->
      meck:expect(hackney, request,
                  fun(get, _Url, [], <<>>, [with_body]) ->
                      {ok, 200, Headers, Body}
                  end),

      ?assertEqual(
         {ok, []},
         octo:list_pull_requests("user", "repo")
      ),
      ?assert(meck:validate(hackney)),

      meck:expect(hackney, request,
                  fun(get, _Url, ReqHeaders, <<>>, [with_body]) ->
                      ?assertEqual(
                         list_to_binary(ETag),
                         proplists:get_value(<<"If-None-Match">>, ReqHeaders)),
                      ?assertEqual(
                         list_to_binary(LastModified),
                         proplists:get_value(<<"If-Modified-Since">>,
                                             ReqHeaders)),
                      {ok, 304, [], ""}
                  end),

      ?assertEqual(
         {ok, []},
         octo:list_pull_requests("user", "repo")
      ),
      ?assert(meck:validate(hackney))
    end]).

request_error_passthrough_test_() ->
  ?HACKNEY_MOCK([
    fun() ->
        meck:expect(hackney, request,
                    fun(_Method, _Url, [], <<>>, [with_body]) ->
                        {error, {closed, <<>>}}
                    end),

        ?assertEqual(
           {error, {closed, <<>>}},
           octo_http_proxy:request(get, url, [])),
        ?assert(meck:validate(hackney))
    end]).

clean_stop_test_() ->
  {setup,
   fun()  -> ok = application:start(octo, transient) end,
   fun(_) -> ok = application:stop(octo) end,
   [?_assertEqual(ok, octo_http_proxy:stop())]}.

ignores_unknown_calls_test_() ->
  {setup,
   fun()  -> ok = application:start(octo) end,
   fun(_) -> ok = application:stop(octo) end,
   % Default timeout for gen_server calls is five seconds. We need to wait just
   % a tad longer so that test doesn't timeout before gen_server does. 10 seems
   % like a good enough value
   [{timeout, 10, fun() ->
        ?assertException(exit,
                         {timeout, _},
                         gen_server:call(octo_http_proxy, nonsensical_call))
    end}]}.

ignores_unknown_casts_test_() ->
  {setup,
   fun()  -> ok = application:start(octo) end,
   fun(_) -> ok = application:stop(octo) end,
   [?_assertEqual(ok, gen_server:cast(octo_http_proxy, nonsensical_cast))]}.

ignores_unknown_messages_test_() ->
  {setup,
   fun()  -> ok = application:start(octo) end,
   fun(_) -> ok = application:stop(octo) end,
   [fun() ->
        Pid = whereis(octo_http_proxy),
        Pid ! hello,
        ?_assertEqual(Pid, whereis(octo_http_proxy))
    end]}.

code_change_test_() ->
  {setup,
   fun()  -> ok = application:start(octo) end,
   fun(_) -> ok = application:stop(octo) end,
   [?_assertEqual({ok, test_state},
                  octo_http_proxy:code_change(version, test_state, extra))]}.
