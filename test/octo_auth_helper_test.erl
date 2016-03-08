-module(octo_auth_helper_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/octo.hrl").

parse_options_test_() ->
  {inparallel,
   [?_assertEqual(
      [],
      octo_auth_helper:parse_options([])),
    ?_assertEqual(
      [{"Authorization", "Basic VGVzdDp4LW9hdXRoLWJhc2lj"}],
      octo_auth_helper:parse_options([{auth, pat, "Test"}])),
    ?_assertEqual(
      [],
      octo_auth_helper:parse_options([unknown]))
   ]}.
