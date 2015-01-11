-module(octo_reference).
-include("octo.hrl").
-export([
  list/3, list_branches/3, list_tags/3, read_tag/4, read_branch/4
]).

%% API

list(Owner, Repo, Options) ->
  References = octo_http_helper:read_collection(reference, [Owner, Repo], Options),
  Result     = [ ?struct_to_record(octo_reference, Reference) || (Reference) <- References ],
  {ok, Result}.

list_branches(Owner, Repo, Options)           -> list_references(branch, Owner, Repo, Options).
list_tags(Owner, Repo, Options)               -> list_references(tag, Owner, Repo, Options).
read_tag(Owner, Repo, TagName, Options)       -> read_reference(tag, Owner, Repo, TagName, Options).
read_branch(Owner, Repo, BranchName, Options) -> read_reference(branch, Owner, Repo, BranchName, Options).

%% Internals

list_references(Type, Owner, Repo, Options) ->
  References = octo_http_helper:read_collection(Type, [Owner, Repo], Options),
  Result     = [ struct_to_record(Reference) || (Reference) <- References ],
  {ok, Result}.

read_reference(Type, Owner, Repo, RefName, Options) ->
  Fun           = list_to_atom(atom_to_list(Type) ++ "_url"),
  Url           = erlang:apply(octo_url_helper, Fun, [Owner, Repo, RefName]),
  {State, Json} = octo_http_helper:get(Url, Options),
  case State of
    ok -> {ok, struct_to_record(jsonerl:decode(Json))};
    _  -> {State, Json}
  end.

struct_to_record(Struct) ->
  Record = ?struct_to_record(octo_reference, Struct),
  OldRef = Record#octo_reference.ref,
  NewRef = binary:replace(OldRef, [<<"refs/heads/">>, <<"refs/tags/">>], <<"">>),
  Record#octo_reference{ref=NewRef}.
