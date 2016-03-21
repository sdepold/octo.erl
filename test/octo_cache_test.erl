-module(octo_cache_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/octo.hrl").
-include("test/tests.hrl").

clean_stop_test_() ->
  {setup,
   fun()  -> ok = application:start(octo, transient) end,
   fun(_) -> ok = application:stop(octo) end,
   [?_assertEqual(ok, octo_cache:stop())]}.

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
                         gen_server:call(octo_cache, nonsensical_call))
    end}]}.

ignores_unknown_casts_test_() ->
  {setup,
   fun()  -> ok = application:start(octo) end,
   fun(_) -> ok = application:stop(octo) end,
   [?_assertEqual(ok, gen_server:cast(octo_cache, nonsensical_cast))]}.

ignores_unknown_messages_test_() ->
  {setup,
   fun()  -> ok = application:start(octo) end,
   fun(_) -> ok = application:stop(octo) end,
   [fun() ->
        Pid = whereis(octo_cache),
        Pid ! hello,
        ?_assertEqual(Pid, whereis(octo_cache))
    end]}.

code_change_test_() ->
  {setup,
   fun()  -> ok = application:start(octo) end,
   fun(_) -> ok = application:stop(octo) end,
   [?_assertEqual({ok, test_state},
                  octo_cache:code_change(version, test_state, extra))]}.
