-module(octo_reference).
-include("octo.hrl").
-export([
  list/2, list/3,
  list_branches/2, list_branches/3,
  list_tags/2, list_tags/3,
  read/4,
  read_tag/4,
  read_branch/4,
  create/5, create_branch/5, create_tag/5,
  update/5,
  delete/4, delete_branch/4, delete_tag/4
]).

%% API

list(Arg, Options) ->
  octo_pagination_helper:read_collection(Arg,
                                         Options,
                                         fun process_references/1).
list(Owner, Repo, Options) ->
  Url = octo_url_helper:generate_url(reference, [Owner, Repo], Options),
  octo_http_helper:read_collection(Url, Options, fun process_references/1).

list_branches(Arg, Options) ->
  octo_pagination_helper:read_collection(Arg,
                                         Options,
                                         fun process_references/1).
list_branches(Owner, Repo, Options) ->
  Url = octo_url_helper:generate_url(branch, [Owner, Repo], Options),
  octo_http_helper:read_collection(Url, Options, fun process_references/1).

list_tags(Arg, Options) ->
  octo_pagination_helper:read_collection(Arg,
                                         Options,
                                         fun process_references/1).
list_tags(Owner, Repo, Options) ->
  Url = octo_url_helper:generate_url(tag, [Owner, Repo], Options),
  octo_http_helper:read_collection(Url, Options, fun process_references/1).

read(Owner, Repo, RefName, Options) ->
  read_reference(reference, Owner, Repo, RefName, Options).

read_tag(Owner, Repo, TagName, Options) ->
  read_reference(tag, Owner, Repo, TagName, Options).

read_branch(Owner, Repo, BranchName, Options) ->
  read_reference(branch, Owner, Repo, BranchName, Options).

create(Owner, Repo, Ref, Sha, Options) ->
  Url          = octo_url_helper:reference_url(Owner, Repo),

  RefBinary    = octo_binary_helper:ensure_binary(Ref),
  ShaBinary    = octo_binary_helper:ensure_binary(Sha),
  Payload      = {
                   {<<"ref">>, RefBinary},
                   {<<"sha">>, ShaBinary}
                 },
  PayloadJson  = jsonerl:encode(Payload),
  case octo_http_helper:post(Url, Options, PayloadJson) of
    {ok, Result} -> {ok, ?json_to_record(octo_reference, Result)};
    Other -> Other
  end.

create_branch(Owner, Repo, BranchName, Source, Options) ->
  create(Owner, Repo, "refs/heads/" ++ BranchName, Source , Options).

create_tag(Owner, Repo, TagName, Source, Options) ->
  create(Owner, Repo, "refs/tags/" ++ TagName, Source, Options).

update(Owner, Repo, "refs/" ++ RefName, Sha, Options) ->
  update(Owner, Repo, RefName, Sha, Options);
update(Owner, Repo, RefName, Sha, Options) ->
  Url          = octo_url_helper:reference_url(Owner, Repo, RefName),
  ShaBinary    = octo_binary_helper:ensure_binary(Sha),
  Payload      = {
                   {<<"sha">>, ShaBinary}
                 },
  PayloadJson  = jsonerl:encode(Payload),
  case octo_http_helper:patch(Url, Options, PayloadJson) of
    {ok, Result} -> {ok, ?json_to_record(octo_reference, Result)};
    Other -> Other
  end.

delete(Owner, Repo, "refs/" ++ RefName, Options) ->
  delete(Owner, Repo, RefName, Options);
delete(Owner, Repo, RefName, Options) ->
  Url = octo_url_helper:reference_url(Owner, Repo, RefName),
  octo_http_helper:delete(Url, Options).

delete_branch(Owner, Repo, BranchName, Options) ->
  delete(Owner, Repo, "refs/heads/" ++ BranchName, Options).

delete_tag(Owner, Repo, TagName, Options) ->
  delete(Owner, Repo, "refs/tags/" ++ TagName, Options).

%% Internals

process_references(References) ->
  [ ?struct_to_record(octo_reference, Reference)
             || (Reference) <- References ].

read_reference(Type, Owner, Repo, "refs/" ++ RefName, Options) ->
  read_reference(Type, Owner, Repo, RefName, Options);
read_reference(Type, Owner, Repo, RefName, Options) ->
  Fun = list_to_atom(atom_to_list(Type) ++ "_url"),
  Url = erlang:apply(octo_url_helper, Fun, [Owner, Repo, RefName]),
  read_reference(Url, Options).

read_reference(Url, Options) ->
  case octo_http_helper:get(Url, Options) of
    {ok, cached, CacheKey} -> octo_cache:retrieve({url, CacheKey});
    {ok, Json, CacheKey, CacheEntry} ->
      Result = ?struct_to_record(octo_reference, jsonerl:decode(Json)),
      octo_cache:store(
        CacheKey,
        CacheEntry#octo_cache_entry{result = Result}),
      {ok, Result};
    Other -> Other
  end.
