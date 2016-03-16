-module(octo_cache).
-include("octo.hrl").
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-export([retrieve/1, store/2]).

-record(state, {by_url = dict:new(), by_result = dict:new()}).

%% Public functions

retrieve({url, Key}) ->
  gen_server:call(?MODULE, {retrieve, {url, Key}});
retrieve({result, Key}) ->
  gen_server:call(?MODULE, {retrieve, {result, Key}}).

store(Key, Value) ->
  gen_server:call(?MODULE, {store, Key, Value}).

%% Callbacks

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

init(_Args) ->
  {ok, #state{}}.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call({retrieve, {url, Key}}, _From, State) ->
  case dict:find(Key, State#state.by_url) of
    error -> {reply, {error, not_found}, State};
    {ok, Value} -> {reply, {ok, Value}, State}
  end;
handle_call({retrieve, {result, Key}}, _From, State) ->
  case dict:find(Key, State#state.by_result) of
    error -> {reply, {error, not_found}, State};
    {ok, Value} -> {reply, {ok, Value}, State}
  end;
handle_call({store, Key, Value}, _From, State) ->
  Result = Value#octo_cache_entry.result,

  ByUrl    = dict:store(Key,    Value, State#state.by_url),
  ByResult = dict:store(Result, Value, State#state.by_result),

  NewState = State#state{by_url = ByUrl, by_result = ByResult},

  {reply, ok, NewState};
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
