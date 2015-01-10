-module(octo_http_helper_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/octo.hrl").

options_to_query_params_test() ->
  ?assertEqual(
    octo_http_helper:options_to_query_params([]),
    ""
  ),
  ?assertEqual(
    octo_http_helper:options_to_query_params([{per_page, 100}]),
    "per_page=100"
  ),
  ?assertEqual(
    octo_http_helper:options_to_query_params([{page, 2}]),
    "page=2"
  ),
  ?assertEqual(
    octo_http_helper:options_to_query_params([{per_page, 100}, {page, 2}]),
    "per_page=100&page=2"
  ).
