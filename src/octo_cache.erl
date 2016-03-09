-module(octo_cache).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-export([request/5,
         get_ratelimit/0, get_ratelimit_remaining/0, get_ratelimit_reset/0]).

-record(ratelimit, {limit, remaining, reset}).
-record(cache_state, {ratelimit = #ratelimit{}}).

%% Public functions

request(Method, Url, Headers, Payload, Options) ->
  gen_server:call(?MODULE, {request, Method, Url, Headers, Payload, Options}).

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
  {ok, #cache_state{}}.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call({request, Method, Url, Headers, Payload, Options}, _From, State) ->
  {ok, StatusCode, RespHeaders, ClientRef} =
    hackney:request(Method, Url, Headers, Payload, Options),

  NewState = update_ratelimit(State, RespHeaders),

  Result = {ok, StatusCode, RespHeaders, ClientRef},
  {reply, Result, NewState};
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

update_ratelimit(State, Headers) ->
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
