-module(octo_http_helper).
-export([
  get/2, delete/2, post/3, patch/3, read_collection/3, get_response_status_code/2,
  options_to_query_params/1, put/3
]).

get(Url, Headers) ->
  {ok, StatusCode, _RespHeaders, ClientRef} = do_request(get, Url, Headers),
  {ok, Body} = hackney:body(ClientRef),
  {status_code_to_tuple_state(StatusCode), Body}.

delete(Url, Headers) ->
  {ok, StatusCode, _RespHeaders, _ClientRef} = do_request(delete, Url, Headers),
  {status_code_to_tuple_state(StatusCode), null}.

post(Url, Headers, Payload) ->
  {ok, StatusCode, _RespHeaders, ClientRef} = do_request(post, Url, Headers, Payload),
  {ok, Body} = hackney:body(ClientRef),
  {status_code_to_tuple_state(StatusCode), Body}.

put(Url, Headers, Payload) ->
  {ok, StatusCode, _RespHeaders, ClientRef} = do_request(put, Url, Headers, Payload),
  {ok, Body} = hackney:body(ClientRef),
  {status_code_to_tuple_state(StatusCode), Body}.

patch(Url, Headers, Payload) ->
  {ok, StatusCode, _RespHeaders, ClientRef} = do_request(patch, Url, Headers, Payload),
  {ok, Body} = hackney:body(ClientRef),
  {status_code_to_tuple_state(StatusCode), Body}.

get_response_status_code(Url, Headers) ->
  {ok, StatusCode, _RespHeaders, _ClientRef} = do_request(get, Url, Headers),
  StatusCode.

%% Usage: read_collection(pull_request, [Owner, Repo], Options).
read_collection(Thing, Args, _Options) ->
  Fun        = list_to_atom(atom_to_list(Thing) ++ "_url"),
  Url        = erlang:apply(octo_url_helper, Fun, Args),
  Options    = check_pagination_options(_Options),
  Query      = options_to_query_params(Options),
  FullUrl    = octo_list_helper:join("?", [Url, Query]),
  {ok, Json} = get(FullUrl, Options),
  Result     = jsonerl:decode(Json),
  case continue_read_collection(Options, Result) of
    true  -> Result ++ read_collection(Thing, Args, increase_page(Options));
    false -> Result
  end.

options_to_query_params(Options) ->
  Fragments = options_to_query_params(Options, []),
  octo_list_helper:join("&", Fragments).

%% Internals

status_code_to_tuple_state(StatusCode) ->
  case round(StatusCode / 100) of
    2 -> ok;
    _ -> err
  end.

do_request(Method, Url, Headers) ->
  do_request(Method, Url, Headers, <<>>, []).

do_request(Method, Url, Headers, Payload) ->
  do_request(Method, Url, Headers, Payload, []).

do_request(Method, Url, Headers, Payload, Options) ->
  hackney:start(),
  ParsedHeaders = octo_auth_helper:parse_options(Headers),
  Res = hackney:request(Method, Url, ParsedHeaders, Payload, Options),
  Res.

options_to_query_params([], Query) ->
  Query;
options_to_query_params([{ per_page, PerPage }|Rest], Query) ->
  options_to_query_params(Rest, Query ++ ["per_page=" ++ integer_to_list(PerPage)]);
options_to_query_params([{ page, Page }|Rest], Query) ->
  options_to_query_params(Rest, Query ++ ["page=" ++ integer_to_list(Page)]);
options_to_query_params([_|Rest], Query) ->
  options_to_query_params(Rest, Query).

continue_read_collection(_, []) -> false;
continue_read_collection([], _) -> false;
continue_read_collection([{ all_pages }|_], _) -> true;
continue_read_collection([_|Rest], Result) ->
  continue_read_collection(Rest, Result).

increase_page([]) -> [];
increase_page([{ page, Page } | Rest]) -> [{ page, Page + 1 }] ++ Rest;
increase_page([Head | Rest]) -> [Head | increase_page(Rest)].

has_page_option([]) -> false;
has_page_option([{ page, _}|_]) -> true;
has_page_option([_|Rest]) -> has_page_option(Rest).

has_all_pages_option([]) -> false;
has_all_pages_option([{ all_pages }|_]) -> true;
has_all_pages_option([_|Rest]) -> has_all_pages_option(Rest).

check_pagination_options(Options) ->
  AddPageOption = (not has_page_option(Options)) and has_all_pages_option(Options),
  case AddPageOption of
    true  -> [{ page, 1 } | Options];
    false -> Options
  end.
