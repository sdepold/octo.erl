-module(octo_cache).
-include("octo.hrl").
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-export([request/3, request/4, request/5, body/1,
         get_ratelimit/0, get_ratelimit_remaining/0, get_ratelimit_reset/0,
         store/2, retrieve/1, update/3, insert_or_update/3]).

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

store(Key, Value) ->
  gen_server:call(?MODULE, {store, Key, Value}).

retrieve(Key) ->
  case ets:lookup(octo_cache_general, Key) of
    [{Key, Value}] -> {ok, Value};
    _              -> {error, not_found}
  end.

update(Key, FieldNo, Value) ->
  gen_server:call(?MODULE, {update, Key, FieldNo, Value}).

insert_or_update(Key, FieldNo, Value) ->
  gen_server:call(?MODULE, {insert_or_update, Key, FieldNo, Value}).

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

      RequestRef = make_ref(),
      %% Asserting that the function returns anything but 'false'. That ensures
      %% that insert didn't replace anything.
      true = false =/=
      ets:insert_new(octo_cache_client_refs, {RequestRef, ClientRef}),

      Result = {ok, StatusCode, RespHeaders, RequestRef},

      {reply, Result, NewState};
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
handle_call({store, Key, Value}, _From, State) ->
  ok = dangerous_store(Key, Value),
  {reply, ok, State};
handle_call({update, Key, FieldNo, Value}, _From, State) ->
  Result = dangerous_update(Key, FieldNo, Value),
  {reply, Result, State};
handle_call({insert_or_update, Key, FieldNo, Value}, _From, State) ->
  Result = dangerous_insert_or_update(Key, FieldNo, Value),
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
    [{ok, Value}] ->
      Headers  = case Value#octo_cache_headers.etag of
                   undefined -> [];
                   ETag -> [{<<"If-None-Match">>, ETag}]
                 end,
      Headers2 = case Value#octo_cache_headers.last_modified of
                   undefined -> Headers;
                   LM -> [{<<"If-Modified-Since">>, LM} | Headers]
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
       ok = dangerous_insert_or_update(
              CacheKey,
              #octo_cache_entry.headers,
              [{<<"ETag">>, ETag}, {<<"Last-Modified">>, Last_Modified}]);
       true -> ok
  end.

dangerous_store(Key, Value) ->
  true = ets:insert(octo_cache_general, {Key, Value}),
  ok.

dangerous_update(Key, FieldNo, Value) ->
  case retrieve(Key) of
    {ok, CachedValue} ->
      UpdatedValue = setelement(FieldNo, CachedValue, Value),
      ok = dangerous_store(Key, UpdatedValue);
    Other -> Other
  end.

dangerous_insert_or_update(Key, FieldNo, Value) ->
  case dangerous_update(Key, FieldNo, Value) of
    ok -> ok;
    _  -> dangerous_store(Key, setelement(FieldNo, #octo_cache_entry{}, Value))
  end.
