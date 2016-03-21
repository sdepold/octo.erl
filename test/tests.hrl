% Path to directory containing assets for tests; relative to `.eunit` which is
% in the root of the project
-define(ASSETS_DIR, "../test/assets/").

-define(HACKNEY_MOCK(Tests),
        {inorder,
         {foreach,
          fun() ->
              ok = application:start(octo, transient),
              ok = meck:new(hackney),
              meck:expect(hackney, start, fun() -> ok end)
          end,
          fun(_) ->
              ok = meck:unload(hackney),
              ok = application:stop(octo)
          end,
          Tests}}).
