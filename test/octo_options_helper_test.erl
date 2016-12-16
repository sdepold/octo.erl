-module(octo_options_helper_test).
-include_lib("eunit/include/eunit.hrl").
-include("include/octo.hrl").

extract_payload_test_() ->
  Proplist = [ {body, "hello world!"},
               {state, closed},
               {number, 42},
               {unused, undefined}
             ],

  {inparallel,
   [?_assertEqual(
       Result,
       octo_options_helper:extract_payload(Proplist, Fields))
    ||
    {Fields, Result} <-
    [{ [body, number],
       { {<<"body">>, <<"hello world!">>},
         {<<"number">>, 42}}},
     { [foo, bar, baz],
       {}}]
    ]}.
