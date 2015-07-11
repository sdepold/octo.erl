-module(octo_http_helper_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/octo.hrl").

options_to_query_params_test() ->
  ?assertEqual(
    "",
    octo_http_helper:options_to_query_params([])
  ),
  ?assertEqual(
    "per_page=100",
    octo_http_helper:options_to_query_params([{per_page, 100}])
  ),
  ?assertEqual(
    "page=2",
    octo_http_helper:options_to_query_params([{page, 2}])
  ),
  ?assertEqual(
    "per_page=100&page=2",
    octo_http_helper:options_to_query_params([{per_page, 100}, {page, 2}])
  ).
