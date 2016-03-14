-module(octo_auth_helper).
-export([parse_options/1]).

%% Authentication helpers

parse_options([]) ->
  [];

%% Authentication via Personal API token: https://github.com/blog/1509-personal-api-tokens
parse_options([{ auth, pat, Pat } | _Rest]) ->
  Header = {"Authorization", "Basic " ++ base64:encode_to_string(Pat ++ ":x-oauth-basic")},
  [Header];

parse_options([_|Rest]) ->
  parse_options(Rest).
