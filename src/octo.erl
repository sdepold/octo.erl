-module(octo).

%% octo: octo library's entry point.

-export([
  list_pull_requests/2, list_pull_requests/3,
  read_pull_request/3, read_pull_request/4,
  list_pull_request_commits/3, list_pull_request_commits/4,
  list_pull_request_files/3, list_pull_request_files/4,
  is_pull_request_merged/3, is_pull_request_merged/4,
  create_pull_request/3, create_pull_request/4,
  update_pull_request/4, update_pull_request/5,
  merge_pull_request/3, merge_pull_request/4
]).

-export([
  list_references/2, list_references/3,
  list_branches/2, list_branches/3,
  list_tags/2, list_tags/3,
  read_reference/3, read_reference/4,
  read_tag/3, read_tag/4,
  read_branch/3, read_branch/4,
  create_reference/3, create_reference/4, create_branch/4, create_branch/5, create_tag/4, create_tag/5,
  update_reference/4, update_reference/5,
  delete_reference/3, delete_reference/4, delete_branch/3, delete_branch/4, delete_tag/3, delete_tag/4
]).

%% API

%%% Pull Requests

list_pull_requests(User, Repo) -> list_pull_requests(User, Repo, []).
list_pull_requests(User, Repo, Options) -> exec(octo_pull_request, list, [User, Repo, Options]).

read_pull_request(User, Repo, Number) -> read_pull_request(User, Repo, Number, []).
read_pull_request(User, Repo, Number, Options) -> exec(octo_pull_request, read, [User, Repo, Number, Options]).

list_pull_request_commits(User, Repo, Number) -> list_pull_request_commits(User, Repo, Number, []).
list_pull_request_commits(User, Repo, Number, Options) -> exec(octo_pull_request, list_commits, [User, Repo, Number, Options]).

list_pull_request_files(User, Repo, Number) -> list_pull_request_files(User, Repo, Number, []).
list_pull_request_files(User, Repo, Number, Options) -> exec(octo_pull_request, list_files, [User, Repo, Number, Options]).

is_pull_request_merged(User, Repo, Number) -> is_pull_request_merged(User, Repo, Number, []).
is_pull_request_merged(User, Repo, Number, Options) -> exec(octo_pull_request, is_merged, [User, Repo, Number, Options]).

create_pull_request(User, Repo, Payload) -> create_pull_request(User, Repo, Payload, []).
create_pull_request(User, Repo, Payload, Options) -> exec(octo_pull_request, create, [User, Repo, Payload, Options]).

update_pull_request(User, Repo, Number, Payload) -> update_pull_request(User, Repo, Number, Payload, []).
update_pull_request(User, Repo, Number, Payload, Options) -> exec(octo_pull_request, update, [User, Repo, Number, Payload, Options]).

merge_pull_request(User, Repo, Number) -> merge_pull_request(User, Repo, Number, []).
merge_pull_request(User, Repo, Number, Options) -> exec(octo_pull_request, merge, [User, Repo, Number, Options]).

%%% References

list_references(User, Repo) -> list_references(User, Repo, []).
list_references(User, Repo, Options) -> exec(octo_reference, list, [User, Repo, Options]).

list_branches(User, Repo) -> list_branches(User, Repo, []).
list_branches(User, Repo, Options) -> exec(octo_reference, list_branches, [User, Repo, Options]).

list_tags(User, Repo) -> list_tags(User, Repo, []).
list_tags(User, Repo, Options) -> exec(octo_reference, list_tags, [User, Repo, Options]).

read_reference(User, Repo, RefName) -> read_reference(User, Repo, RefName, []).
read_reference(User, Repo, RefName, Options) -> exec(octo_reference, read, [User, Repo, RefName, Options]).

read_tag(User, Repo, TagName) -> read_tag(User, Repo, TagName, []).
read_tag(User, Repo, TagName, Options) -> exec(octo_reference, read_tag, [User, Repo, TagName, Options]).

read_branch(User, Repo, BranchName) -> read_branch(User, Repo, BranchName, []).
read_branch(User, Repo, BranchName, Options) -> exec(octo_reference, read_branch, [User, Repo, BranchName, Options]).

create_reference(User, Repo, Payload) -> create_reference(User, Repo, Payload, []).
create_reference(User, Repo, Payload, Options) -> exec(octo_reference, create, [User, Repo, Payload, Options]).

create_branch(User, Repo, BranchName, Source) -> create_branch(User, Repo, BranchName, Source, []).
create_branch(User, Repo, BranchName, Source, Options) -> exec(octo_reference, create_branch, [User, Repo, BranchName, Source, Options]).

create_tag(User, Repo, TagName, Source) -> create_tag(User, Repo, TagName, Source, []).
create_tag(User, Repo, TagName, Source, Options) -> exec(octo_reference, create_tag, [User, Repo, TagName, Source, Options]).

update_reference(User, Repo, RefName, Payload) -> update_reference(User, Repo, RefName, Payload, []).
update_reference(User, Repo, RefName, Payload, Options) -> exec(octo_reference, update, [User, Repo, RefName, Payload, Options]).

delete_reference(User, Repo, RefName) -> delete_reference(User, Repo, RefName, []).
delete_reference(User, Repo, RefName, Options) -> exec(octo_reference, delete, [User, Repo, RefName, Options]).

delete_branch(User, Repo, BranchName) -> delete_branch(User, Repo, BranchName, []).
delete_branch(User, Repo, BranchName, Options) -> exec(octo_reference, delete_branch, [User, Repo, BranchName, Options]).

delete_tag(User, Repo, TagName) -> delete_tag(User, Repo, TagName, []).
delete_tag(User, Repo, TagName, Options) -> exec(octo_reference, delete_tag, [User, Repo, TagName, Options]).

%% Internals

exec(Mod, Fun, Args) ->
  erlang:apply(Mod, Fun, Args).

%% End of Module.
