-module(octo_cache).
-include("octo.hrl").
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-export([retrieve/1, update_cache/2]).

%% Public functions

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
  ets:new(octo_cache_general, [protected, named_table]),

  {ok, undefined}.

handle_call(stop, _From, State) ->
  {stop, normal, ok, State};
handle_call({update_cache, Key, UpdateFun}, _From, State) ->
  CurrentValue = case retrieve(Key) of
                   {ok, CachedValue}  -> CachedValue;
                   {error, not_found} -> #octo_cache_entry{}
                 end,
  NewValue = UpdateFun(CurrentValue),
  true = ets:insert(octo_cache_general, {Key, NewValue}),
  {reply, ok, State};
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
