-module(octo_http_helper).
-include("octo.hrl").
-export([
  get/2, delete/2, post/3, patch/3, read_collection/3, get_response_status_code/2,
  options_to_query_params/1, put/3
]).

get(Url, OctoOptions) ->
  Options = [{cache_key, Url} | OctoOptions],

  case octo_http_proxy:request(get, Url, Options) of
    {ok, StatusCode, _RespHeaders, Body, CacheKey, CacheEntry} ->
        case status_code_to_tuple_state(StatusCode) of
          ok  -> {ok,  Body, CacheKey, CacheEntry};
          err -> {err, Body}
        end;
    Other -> Other
  end.

delete(Url, OctoOptions) ->
  {ok, StatusCode, _RespHeaders, _Body, _CacheKey, _CacheEntry} =
    octo_http_proxy:request(delete, Url, OctoOptions),
  {status_code_to_tuple_state(StatusCode), null}.

post(Url, OctoOptions, Payload) ->
  {ok, StatusCode, _RespHeaders, Body, _CacheKey, _CacheEntry} =
    octo_http_proxy:request(post, Url, OctoOptions, Payload),
  {status_code_to_tuple_state(StatusCode), Body}.

put(Url, OctoOptions, Payload) ->
  {ok, StatusCode, _RespHeaders, Body, _CacheKey, _CacheEntry} =
    octo_http_proxy:request(put, Url, OctoOptions, Payload),
  {status_code_to_tuple_state(StatusCode), Body}.

patch(Url, OctoOptions, Payload) ->
  {ok, StatusCode, _RespHeaders, Body, _CacheKey, _CacheEntry} =
    octo_http_proxy:request(patch, Url, OctoOptions, Payload),
  {status_code_to_tuple_state(StatusCode), Body}.

get_response_status_code(Url, OctoOptions) ->
  {ok, StatusCode, _RespHeaders, _Body, _CacheKey, _CacheEntry} =
    octo_http_proxy:request(head, Url, OctoOptions),
  {ok, StatusCode}.

%% Usage: read_collection(pull_request, [Owner, Repo], Options).
read_collection(Thing, Args, _Options) ->
  Fun     = list_to_atom(atom_to_list(Thing) ++ "_url"),
  Url     = erlang:apply(octo_url_helper, Fun, Args),
  Options = check_pagination_options(_Options),
  Query   = options_to_query_params(Options),
  FullUrl = octo_list_helper:join("?", [Url, Query]),

  Result = case get(FullUrl, Options) of
             {ok, cached, CacheKey} ->
               octo_cache:retrieve({url, CacheKey});
             {ok, Json, CacheKey, CacheEntry} ->
               Processed = jsonerl:decode(Json),

               octo_cache:store(
                 CacheKey,
                 CacheEntry#octo_cache_entry{result = Processed}),

               Processed;
             Other -> Other
           end,

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

options_to_query_params([], Query) ->
  Query;
options_to_query_params([{ per_page, PerPage }|Rest], Query) ->
  options_to_query_params(Rest, Query ++ ["per_page=" ++ integer_to_list(PerPage)]);
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
