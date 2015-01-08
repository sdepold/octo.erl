-module(octo).

%% octo: octo library's entry point.

-export([
  list_pull_requests/2, list_pull_requests/3,
  read_pull_request/3, read_pull_request/4,
  list_pull_request_commits/3, list_pull_request_commits/4,
  list_pull_request_files/3, list_pull_request_files/4,
  is_pull_request_merged/3, is_pull_request_merged/4,
  create_pull_request/3, create_pull_request/4,
  update_pull_request/4, update_pull_request/5
]).

-export([
  list_branches/2, list_branches/3,
  list_tags/2, list_tags/3
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

is_pull_request_merged(User, Repo, Number) ->
  is_pull_request_merged(User, Repo, Number, []).
is_pull_request_merged(User, Repo, Number, Options) ->
  exec(octo_pull_request, is_merged, [User, Repo, Number, Options]).

create_pull_request(User, Repo, Payload) ->
  create_pull_request(User, Repo, Payload, []).
create_pull_request(User, Repo, Payload, Options) ->
  exec(octo_pull_request, create, [User, Repo, Payload, Options]).

update_pull_request(User, Repo, Number, Payload) ->
  update_pull_request(User, Repo, Number, Payload, []).
update_pull_request(User, Repo, Number, Payload, Options) ->
  exec(octo_pull_request, update, [User, Repo, Number, Payload, Options]).

list_branches(User, Repo) ->
  list_branches(User, Repo, []).
list_branches(User, Repo, Options) ->
  exec(octo_reference, list_branches, [User, Repo, Options]).

list_tags(User, Repo) ->
  list_tags(User, Repo, []).
list_tags(User, Repo, Options) ->
  exec(octo_reference, list_tags, [User, Repo, Options]).

%% Internals

exec(Mod, Fun, Args) ->
  erlang:apply(Mod, Fun, Args).

%% End of Module.
