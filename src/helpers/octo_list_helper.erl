-module(octo_list_helper).
-export([join/2]).

join(Join, L) ->
  join(Join, L, fun(E) -> E end).
join(_Join, L=[], _Conv) ->
  L;
join(Join, [H|Q], Conv) ->
  lists:flatten(lists:concat(
    [Conv(H)|lists:map(fun(E) -> [Join, Conv(E)] end, Q)]
  )).
