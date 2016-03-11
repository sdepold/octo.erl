-module(octo_cache).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-export([request/5, body/1,
         get_ratelimit/0, get_ratelimit_remaining/0, get_ratelimit_reset/0]).

-record(ratelimit, {limit, remaining, reset}).
-record(cache_state, {ratelimit = #ratelimit{}}).

-record(request, {method, url}).
-record(caching_headers, {etag, last_modified}).

%% Public functions

request(Method, Url, Headers, Payload, Options) ->
  gen_server:call(?MODULE, {request, Method, Url, Headers, Payload, Options}).

body(RequestRef) ->
  gen_server:call(?MODULE, {body, RequestRef}).

get_ratelimit() ->
  gen_server:call(?MODULE, {get_ratelimit}).

get_ratelimit_remaining() ->
  gen_server:call(?MODULE, {get_ratelimit_remaining}).

get_ratelimit_reset() ->
  gen_server:call(?MODULE, {get_ratelimit_reset}).

%% Callbacks

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

init(_Args) ->
  ets:new(octo_cache_client_refs, [private, named_table]),
  ets:new(octo_cache_headers,     [private, named_table]),

  {ok, #cache_state{}}.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call({request, Method, Url, Headers, Payload, Options}, _From, State) ->
  Headers2 = add_caching_headers(Headers, Method, Url),
  case hackney:request(Method, Url, Headers2, Payload, Options) of
    {ok, StatusCode, RespHeaders, ClientRef} ->
      NewState = update_ratelimit(RespHeaders, State),

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

add_caching_headers(Headers, Method, Url) ->
  Pred = fun({Name, _}) ->
             (Name =:= <<"ETag">>) orelse (Name =:= <<"Last-Modified">>)
         end,
  NotAlreadySet = not lists:any(Pred, Headers),
  if
    NotAlreadySet ->
      Key = #request{method = Method, url = Url},
      case ets:lookup(octo_cache_headers, Key) of
        %% Found some values for the headers; let's use them
        [{Key, Value}] ->
          Headers2 = case Value#caching_headers.etag of
                       undefined -> Headers;
                       ETag -> [{<<"If-None-Match">>, ETag} | Headers]
                     end,
          Headers3 = case Value#caching_headers.last_modified of
                       undefined -> Headers2;
                       LM -> [{<<"If-Modified-Since">>, LM} | Headers2]
                     end,
          Headers3;
        %% We don't have any headers stored for this request
        _ -> Headers
      end;
    true ->
      %% At least one of the headers is already set; not touching anything!
      Headers
  end.
