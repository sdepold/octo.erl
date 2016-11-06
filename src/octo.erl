-module(octo).

%% octo: octo library's entry point.

-export([set_credentials/2,
         get_ratelimit/0, get_ratelimit_remaining/0, get_ratelimit_reset/0]).

-export([
  list_pull_requests/1, list_pull_requests/2, list_pull_requests/3,
  read_pull_request/3, read_pull_request/4,
  list_pull_request_commits/1, list_pull_request_commits/2,
  list_pull_request_commits/3, list_pull_request_commits/4,
  list_pull_request_files/1, list_pull_request_files/2,
  list_pull_request_files/3, list_pull_request_files/4,
  is_pull_request_merged/3, is_pull_request_merged/4,
  create_pull_request/5, create_pull_request/6,
  update_pull_request/4,
  merge_pull_request/3, merge_pull_request/4
]).

-export([
  list_references/1, list_references/2, list_references/3,
  list_branches/1, list_branches/2, list_branches/3,
  list_tags/1, list_tags/2, list_tags/3,
  read_reference/3, read_reference/4,
  read_tag/3, read_tag/4,
  read_branch/3, read_branch/4,
  create_reference/4, create_reference/5,
  create_branch/4, create_branch/5,
  create_tag/4, create_tag/5,
  update_reference/4, update_reference/5,
  delete_reference/3, delete_reference/4,
  delete_branch/3, delete_branch/4,
  delete_tag/3, delete_tag/4
]).

-export([
  list_my_organizations/0, list_my_organizations/1, list_my_organizations/2,
  list_user_organizations/1, list_user_organizations/2,
  read_organization/1, read_organization/2,
  update_organization/2, update_organization/3
]).

%% API

%%% General

set_credentials(Type, Data) ->
  octo_http_proxy:set_credentials(Type, Data).

get_ratelimit() ->
  octo_http_proxy:get_ratelimit().

get_ratelimit_remaining() ->
  octo_http_proxy:get_ratelimit_remaining().

get_ratelimit_reset() ->
  octo_http_proxy:get_ratelimit_reset().

%%% Pull Requests

list_pull_requests(Arg) ->
  list_pull_requests(Arg, []).
list_pull_requests(Arg, Options) when is_tuple(Arg) and is_list(Options) ->
  exec(octo_pull_request, list, [Arg, Options]);
list_pull_requests(Owner, Repo) when is_list(Owner) and is_list(Repo) ->
  list_pull_requests(Owner, Repo, []).
list_pull_requests(Owner, Repo, Options) ->
  exec(octo_pull_request, list, [Owner, Repo, Options]).

read_pull_request(Owner, Repo, Number) ->
  read_pull_request(Owner, Repo, Number, []).
read_pull_request(Owner, Repo, Number, Options) ->
  exec(octo_pull_request, read, [Owner, Repo, Number, Options]).

list_pull_request_commits(Arg) ->
  list_pull_request_commits(Arg, []).
list_pull_request_commits(Arg, Options) ->
  exec(octo_pull_request, list_commits, [Arg, Options]).
list_pull_request_commits(Owner, Repo, Number) ->
  list_pull_request_commits(Owner, Repo, Number, []).
list_pull_request_commits(Owner, Repo, Number, Options) ->
  exec(octo_pull_request, list_commits, [Owner, Repo, Number, Options]).

list_pull_request_files(Arg) ->
  list_pull_request_files(Arg, []).
list_pull_request_files(Arg, Options) ->
  exec(octo_pull_request, list_files, [Arg, Options]).
list_pull_request_files(Owner, Repo, Number) ->
  list_pull_request_files(Owner, Repo, Number, []).
list_pull_request_files(Owner, Repo, Number, Options) ->
  exec(octo_pull_request, list_files, [Owner, Repo, Number, Options]).

is_pull_request_merged(Owner, Repo, Number) ->
  is_pull_request_merged(Owner, Repo, Number, []).
is_pull_request_merged(Owner, Repo, Number, Options) ->
  exec(octo_pull_request, is_merged, [Owner, Repo, Number, Options]).

create_pull_request(Owner, Repo, Title, Head, Base) ->
  create_pull_request(Owner, Repo, Title, Head, Base, []).
create_pull_request(Owner, Repo, Title, Head, Base, Options) ->
  exec(octo_pull_request, create, [Owner, Repo, Title, Head, Base, Options]).

update_pull_request(Owner, Repo, Number, Options) ->
  exec(octo_pull_request, update, [Owner, Repo, Number, Options]).

merge_pull_request(Owner, Repo, Number) ->
  merge_pull_request(Owner, Repo, Number, []).
merge_pull_request(Owner, Repo, Number, Options) ->
  exec(octo_pull_request, merge, [Owner, Repo, Number, Options]).

%%% References

list_references(Arg) ->
  list_references(Arg, []).
list_references(Arg, Options) when is_tuple(Arg) and is_list(Options) ->
  exec(octo_reference, list, [Arg, Options]);
list_references(Owner, Repo) when is_list(Owner) and is_list(Repo) ->
  list_references(Owner, Repo, []).
list_references(Owner, Repo, Options) ->
  exec(octo_reference, list, [Owner, Repo, Options]).

list_branches(Arg) ->
  list_branches(Arg, []).
list_branches(Arg, Options) when is_tuple(Arg) and is_list(Options) ->
  exec(octo_reference, list_branches, [Arg, Options]);
list_branches(Owner, Repo) when is_list(Owner) and is_list(Repo) ->
  list_branches(Owner, Repo, []).
list_branches(Owner, Repo, Options) ->
  exec(octo_reference, list_branches, [Owner, Repo, Options]).

list_tags(Arg) ->
  list_tags(Arg, []).
list_tags(Arg, Options) when is_tuple(Arg) and is_list(Options) ->
  exec(octo_reference, list_tags, [Arg, Options]);
list_tags(Owner, Repo) when is_list(Owner) and is_list(Repo) ->
  list_tags(Owner, Repo, []).
list_tags(Owner, Repo, Options) ->
  exec(octo_reference, list_tags, [Owner, Repo, Options]).

read_reference(Owner, Repo, RefName) ->
  read_reference(Owner, Repo, RefName, []).
read_reference(Owner, Repo, RefName, Options) ->
  exec(octo_reference, read, [Owner, Repo, RefName, Options]).

read_tag(Owner, Repo, TagName) ->
  read_tag(Owner, Repo, TagName, []).
read_tag(Owner, Repo, TagName, Options) ->
  exec(octo_reference, read_tag, [Owner, Repo, TagName, Options]).

read_branch(Owner, Repo, BranchName) ->
  read_branch(Owner, Repo, BranchName, []).
read_branch(Owner, Repo, BranchName, Options) ->
  exec(octo_reference, read_branch, [Owner, Repo, BranchName, Options]).

create_reference(Owner, Repo, Ref, Sha) ->
  create_reference(Owner, Repo, Ref, Sha, []).
create_reference(Owner, Repo, Ref, Sha, Options) ->
  exec(octo_reference, create, [Owner, Repo, Ref, Sha, Options]).

create_branch(Owner, Repo, BranchName, Source) ->
  create_branch(Owner, Repo, BranchName, Source, []).
create_branch(Owner, Repo, BranchName, Source, Options) ->
  exec(octo_reference, create_branch, [Owner, Repo, BranchName, Source, Options]).

create_tag(Owner, Repo, TagName, Source) ->
  create_tag(Owner, Repo, TagName, Source, []).
create_tag(Owner, Repo, TagName, Source, Options) ->
  exec(octo_reference, create_tag, [Owner, Repo, TagName, Source, Options]).

update_reference(Owner, Repo, RefName, Sha) ->
  update_reference(Owner, Repo, RefName, Sha, []).
update_reference(Owner, Repo, RefName, Sha, Options) ->
  exec(octo_reference, update, [Owner, Repo, RefName, Sha, Options]).

delete_reference(Owner, Repo, RefName) ->
  delete_reference(Owner, Repo, RefName, []).
delete_reference(Owner, Repo, RefName, Options) ->
  exec(octo_reference, delete, [Owner, Repo, RefName, Options]).

delete_branch(Owner, Repo, BranchName) ->
  delete_branch(Owner, Repo, BranchName, []).
delete_branch(Owner, Repo, BranchName, Options) ->
  exec(octo_reference, delete_branch, [Owner, Repo, BranchName, Options]).

delete_tag(Owner, Repo, TagName) ->
  delete_tag(Owner, Repo, TagName, []).
delete_tag(Owner, Repo, TagName, Options) ->
  exec(octo_reference, delete_tag, [Owner, Repo, TagName, Options]).

%% Organizations

list_my_organizations() ->
  list_my_organizations([]).
list_my_organizations(Arg) when is_tuple(Arg) ->
  list_my_organizations(Arg, []);
list_my_organizations(Options) when is_list(Options) ->
  exec(octo_organization, list_my_organizations, [Options]).
list_my_organizations(Arg, Options) when is_tuple(Arg) and is_list(Options) ->
  exec(octo_organization, list_my_organizations, [Arg, Options]).

list_user_organizations(Arg) when is_tuple(Arg) ->
  list_user_organizations(Arg, []);
list_user_organizations(User) when is_list(User) ->
  list_user_organizations(User, []).
list_user_organizations(Arg, Options) when is_tuple(Arg) and is_list(Options) ->
  exec(octo_organization, list_user_organizations, [Arg, Options]);
list_user_organizations(User, Options) when is_list(User) and is_list(Options) ->
  exec(octo_organization, list_user_organizations, [User, Options]).

read_organization(Organization) ->
  read_organization(Organization, []).
read_organization(Organization, Options) ->
  exec(octo_organization, read_organization, [Organization, Options]).

update_organization(Organization, Payload) ->
  update_organization(Organization, Payload, []).
update_organization(Organization, Payload, Options) ->
  exec(octo_organization, update_organization, [Organization,
                                                Payload,
                                                Options]).

%% Internals

exec(Mod, Fun, Args) ->
  erlang:apply(Mod, Fun, Args).

%% End of Module.
