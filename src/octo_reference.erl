-module(octo_reference).
-include("octo.hrl").
-export([
  list/3, list_branches/3, list_tags/3,
  read/4, read_tag/4, read_branch/4,
  create/4,
  delete/4
]).
-include_lib("eunit/include/eunit.hrl").

%% API

list(Owner, Repo, Options) -> list_references(reference, Owner, Repo, [{ skip_ref_modification }|Options]).
list_branches(Owner, Repo, Options) -> list_references(branch, Owner, Repo, Options).
list_tags(Owner, Repo, Options) -> list_references(tag, Owner, Repo, Options).

read(Owner, Repo, RefName, Options) -> read_reference(reference, Owner, Repo, RefName, [{ skip_ref_modification }|Options]).
read_tag(Owner, Repo, TagName, Options) -> read_reference(tag, Owner, Repo, TagName, Options).
read_branch(Owner, Repo, BranchName, Options) -> read_reference(branch, Owner, Repo, BranchName, Options).

create(Owner, Repo, Payload, Options) ->
  Url          = octo_url_helper:reference_url(Owner, Repo),
  PayloadJson  = jsonerl:encode(Payload),
  {ok, Result} = octo_http_helper:post(Url, Options, PayloadJson),
  {ok, ?json_to_record(octo_reference, Result)}.

delete(Owner, Repo, RefName, Options) ->
  Url = octo_url_helper:reference_url(Owner, Repo, RefName),
  octo_http_helper:delete(Url, Options).

%% Internals

list_references(Type, Owner, Repo, Options) ->
  References = octo_http_helper:read_collection(Type, [Owner, Repo], Options),
  Result     = [ struct_to_record(Reference, Options) || (Reference) <- References ],
  {ok, Result}.

read_reference(Type, Owner, Repo, RefName, Options) ->
  Fun           = list_to_atom(atom_to_list(Type) ++ "_url"),
  Url           = erlang:apply(octo_url_helper, Fun, [Owner, Repo, RefName]),
  {State, Json} = octo_http_helper:get(Url, Options),
  case State of
    ok -> {ok, struct_to_record(jsonerl:decode(Json), Options)};
    _  -> {State, Json}
  end.

struct_to_record(Struct, [{ skip_ref_modification }|_]) ->
  ?struct_to_record(octo_reference, Struct);

struct_to_record(Struct, _) ->
  Record = ?struct_to_record(octo_reference, Struct),
  OldRef = Record#octo_reference.ref,
  NewRef = binary:replace(OldRef, [<<"refs/heads/">>, <<"refs/tags/">>], <<"">>),
  Record#octo_reference{ref=NewRef}.
