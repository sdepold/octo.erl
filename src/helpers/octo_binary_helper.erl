-module(octo_binary_helper).
-export([ensure_binary/1,
        ensure_binary_if_list/1]).

ensure_binary_if_list(Input) when is_list(Input) ->
  ensure_binary(Input);
ensure_binary_if_list(Input) ->
  Input.

ensure_binary(Input) ->
  if
    is_binary(Input) -> Input;
    true -> unicode:characters_to_binary(Input)
  end.
