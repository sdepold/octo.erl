-module(octo_cache).
-include("octo.hrl").
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-export([request/3, request/4, request/5, body/1,
         get_ratelimit/0, get_ratelimit_remaining/0, get_ratelimit_reset/0,
         retrieve/1, update_cache/2]).

-record(ratelimit, {limit, remaining, reset}).
-record(cache_state, {ratelimit = #ratelimit{}}).

%% Public functions

request(Method, Url, OctoOpts) ->
  request(Method, Url, OctoOpts, <<>>).
request(Method, Url, OctoOpts, Payload) ->
  request(Method, Url, OctoOpts, Payload, []).
request(Method, Url, OctoOpts, Payload, Options) ->
  gen_server:call(?MODULE, {request, Method, Url, OctoOpts, Payload, Options}).

body(RequestRef) ->
  gen_server:call(?MODULE, {body, RequestRef}).

get_ratelimit() ->
  gen_server:call(?MODULE, {get_ratelimit}).

get_ratelimit_remaining() ->
  gen_server:call(?MODULE, {get_ratelimit_remaining}).

get_ratelimit_reset() ->
  gen_server:call(?MODULE, {get_ratelimit_reset}).

retrieve(Key) ->
  case ets:lookup(octo_cache_general, Key) of
    [{Key, Value}] -> {ok, Value};
    _              -> {error, not_found}
  end.

update_cache(Key, UpdateFun) ->
  gen_server:call(?MODULE, {update_cache, Key, UpdateFun}).

%% Callbacks

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

init(_Args) ->
  ets:new(octo_cache_client_refs, [private,   named_table]),
  ets:new(octo_cache_general,     [protected, named_table]),

  {ok, #cache_state{}}.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call({request, Method, Url, OctoOpts, Payload, Opts}, _From, State) ->
  CacheKey = proplists:get_value(cache_key, OctoOpts),

  AuthHeaders = octo_auth_helper:parse_options(OctoOpts),
  CachingHeaders = get_caching_headers(CacheKey),
  Headers = AuthHeaders ++ CachingHeaders,

  case hackney:request(Method, Url, Headers, Payload, Opts) of
    {ok, StatusCode, RespHeaders, ClientRef} ->
      NewState = update_ratelimit(RespHeaders, State),

      store_caching_headers(CacheKey, RespHeaders),

      case StatusCode of
        304 ->
          %% We have to skip the body, otherwise Hackney will keep the
          %% corresponding socket reserved and we'll exhaust the pool
          %% eventually
          ok = hackney:skip_body(ClientRef),
          {reply, {ok, cached}, NewState};
        _ ->
          RequestRef = make_ref(),
          %% Asserting that the function returns anything but 'false'. That
          %% ensures that insert didn't replace anything.
          true = false =/=
          ets:insert_new(octo_cache_client_refs, {RequestRef, ClientRef}),

          Result = {ok, StatusCode, RespHeaders, RequestRef},

          {reply, Result, NewState}
      end;
    Response ->
      {reply, Response, State}
  end;
handle_call({body, RequestRef}, _From, State) ->
  case ets:lookup(octo_cache_client_refs, RequestRef) of
    [{RequestRef, ClientRef}] ->
      Result = hackney:body(ClientRef),
      case Result of
        {error, req_not_found} ->
          ets:delete(octo_cache_client_refs, RequestRef);
        _ -> ok
      end,
      {reply, Result, State};
    _ -> {reply, {error, octo_cache_no_such_ref}, State}
  end;
handle_call({get_ratelimit}, _From, State) ->
  Ratelimit = State#cache_state.ratelimit,
  {reply, Ratelimit#ratelimit.limit, State};
handle_call({get_ratelimit_remaining}, _From, State) ->
  Ratelimit = State#cache_state.ratelimit,
  {reply, Ratelimit#ratelimit.remaining, State};
handle_call({get_ratelimit_reset}, _From, State) ->
  Ratelimit = State#cache_state.ratelimit,
  {reply, Ratelimit#ratelimit.reset, State};
handle_call({update_cache, Key, UpdateFun}, _From, State) ->
  Result = internal_update_cache(Key, UpdateFun),
  {reply, Result, State};
handle_call(_Request, _From, State) ->
  {noreply, State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Helper functions

update_ratelimit(Headers, State) ->
  Ratelimit = State#cache_state.ratelimit,

  Update = fun(FieldNo, HeaderName, RatelimitRecord) ->
                Value = case hackney_headers:parse(HeaderName, Headers) of
                          undefined -> undefined;
                          Binary    -> erlang:list_to_integer(
                                         binary:bin_to_list(Binary))
                        end,

                setelement(FieldNo, RatelimitRecord, Value)
            end,

  NewRatelimit = Update(#ratelimit.limit,     <<"X-RateLimit-Limit">>,
                 Update(#ratelimit.remaining, <<"X-RateLimit-Remaining">>,
                 Update(#ratelimit.reset,     <<"X-RateLimit-Reset">>,
                        Ratelimit))),

  State#cache_state{ratelimit = NewRatelimit}.

get_caching_headers(undefined) -> [];
get_caching_headers(CacheKey) ->
  case retrieve(CacheKey) of
    %% Found some values for the headers; let's use them
    {ok, Value} ->
      Headers  = Value#octo_cache_entry.headers,
      Headers1 = case Headers#octo_cache_headers.etag of
                   undefined -> [];
                   ETag -> [{<<"If-None-Match">>, ETag}]
                 end,
      Headers2 = case Headers#octo_cache_headers.last_modified of
                   undefined -> Headers1;
                   LM -> [{<<"If-Modified-Since">>, LM} | Headers1]
                 end,
      Headers2;
    %% We don't have any headers stored for this request
    _ -> []
  end.

store_caching_headers(undefined, _Headers) -> ok;
store_caching_headers(CacheKey, Headers) ->
  ETag          = hackney_headers:parse(<<"ETag">>, Headers),
  Last_Modified = hackney_headers:parse(<<"Last-Modified">>, Headers),

  %% Don't store anything if both values are undefined
  if (ETag =/= undefined) orelse (Last_Modified =/= undefined) ->
       ok = internal_update_cache(
              CacheKey,
              fun(Entry) ->
                  EntryHeaders = Entry#octo_cache_entry.headers,
                  Updated = EntryHeaders#octo_cache_headers{
                              etag          = ETag,
                              last_modified = Last_Modified},
                  Entry#octo_cache_entry{headers = Updated}
              end);
       true -> ok
  end.

internal_update_cache(Key, UpdateFun) ->
  CurrentValue = case retrieve(Key) of
                   {ok, CachedValue}  -> CachedValue;
                   {error, not_found} -> #octo_cache_entry{}
                 end,
  NewValue = UpdateFun(CurrentValue),
  true = ets:insert(octo_cache_general, {Key, NewValue}),
  ok.
