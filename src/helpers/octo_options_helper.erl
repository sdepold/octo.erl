-module(octo_options_helper).
-export([extract_payload/2]).

%% Extracts values from option proplist, converts them into binaries, then
%% packs everything into one big tuple.
extract_payload(Options, PayloadFields) ->
  % Get the fields we're interested in
  {PayloadValues, _} = proplists:split(Options, PayloadFields),
  PayloadTuples =
    lists:filtermap(
      fun(List) ->
          case List of
            [{Key, Val} | _] ->
              { true,
                {
                 atom_to_binary(Key, unicode),
                 octo_binary_helper:ensure_binary_if_list(Val)
                }
              };
            _ -> false
          end
      end,
      PayloadValues),
  list_to_tuple(PayloadTuples).
