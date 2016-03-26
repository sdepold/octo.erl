-module(octo_http_helper).
-include("octo.hrl").
-export([
  get/2, delete/2, post/3, patch/3, put/3, get_response_status_code/2,
  read_collection/3,
  read_to_record/3
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

read_collection(Url, Options, ProcessingFun) ->
  internal_read_collection(Url, Options, ProcessingFun, []).

read_to_record(Url, Options, ProcessingFun) ->
  case octo_http_helper:get(Url, Options) of
    {ok, cached, CacheKey} ->
      {ok, Entry} = octo_cache:retrieve({url, CacheKey}),
      Entry#octo_cache_entry.result;
    {ok, Json, CacheKey, CacheEntry} ->
      Result = {ok, ProcessingFun(Json)},
      octo_cache:store(
        CacheKey,
        CacheEntry#octo_cache_entry{result = Result}),
      Result;
    {error, Error} -> {error, Error}
  end.

%% Internals

status_code_to_tuple_state(StatusCode) ->
  case round(StatusCode / 100) of
    2 -> ok;
    _ -> err
  end.

internal_read_collection(Url, Options, ProcessingFun, Acc) ->
  Result = case get(Url, Options) of
             {ok, cached, CacheKey} ->
               {ok, Entry} = octo_cache:retrieve({url, CacheKey}),
               Entry#octo_cache_entry.result;
             {ok, Json, CacheKey, CacheEntry} ->
               Processed = ProcessingFun(jsonerl:decode(Json)),
               Res = {ok, Processed},

               octo_cache:store(
                 CacheKey,
                 CacheEntry#octo_cache_entry{result = Res}),

               Res;
             Other -> Other
           end,

  case Result of
    {ok, PrevResult} ->
      case proplists:get_value(all_pages, Options) of
        true ->
          case octo_pagination_helper:get_url({next, Result}) of
            {ok, NextUrl} ->
              internal_read_collection(NextUrl,
                                       Options,
                                       ProcessingFun,
                                       Acc ++ PrevResult);
            {error, no_such_url} ->
              {ok, Acc ++ PrevResult}
          end;
        undefined    ->
          {ok, Acc ++ PrevResult}
      end;
    Other2 -> Other2
  end.
