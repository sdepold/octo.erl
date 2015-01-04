-module(octo).

%% octo: octo library's entry point.

-export([
  list_pull_requests/2, list_pull_requests/3,
  read_pull_request/3, read_pull_request/4,
  list_pull_request_commits/3, list_pull_request_commits/4,
  list_pull_request_files/3, list_pull_request_files/4
]).

%% API

list_pull_requests(User, Repo) ->
  list_pull_requests(User, Repo, []).
list_pull_requests(User, Repo, Options) ->
  exec(octo_pull_request, list, [User, Repo, Options]).

read_pull_request(User, Repo, Number) ->
  read_pull_request(User, Repo, Number, []).
read_pull_request(User, Repo, Number, Options) ->
  exec(octo_pull_request, read, [User, Repo, Number, Options]).

list_pull_request_commits(User, Repo, Number) ->
  list_pull_request_commits(User, Repo, Number, []).
list_pull_request_commits(User, Repo, Number, Options) ->
  exec(octo_pull_request, list_commits, [User, Repo, Number, Options]).

list_pull_request_files(User, Repo, Number) ->
  list_pull_request_files(User, Repo, Number, []).
list_pull_request_files(User, Repo, Number, Options) ->
  exec(octo_pull_request, list_files, [User, Repo, Number, Options]).

%% Internals

exec(Mod, Fun, Args) ->
  hackney:start(),
  Result = erlang:apply(Mod, Fun, Args),
  % hackney:stop(),
  Result.

%% End of Module.
