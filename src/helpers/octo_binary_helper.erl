-module(octo_binary_helper).
-export([ensure_binary/1]).

ensure_binary(Input) ->
  if
    is_binary(Input) -> Input;
    true -> unicode:characters_to_binary(Input)
  end.
