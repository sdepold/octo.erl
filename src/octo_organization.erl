-module(octo_organization).
-include("octo.hrl").
-export([list_my_organizations/1, list_my_organizations/2,
         list_user_organizations/2,
         read_organization/2,
         update_organization/3]).

list_my_organizations(Options) ->
  Url = octo_url_helper:generate_url(my_organizations, [], Options),
  octo_http_helper:read_collection(Url, Options, fun internal_process_orgs/1).
list_my_organizations(Arg, Options) when is_tuple(Arg) and is_list(Options) ->
  octo_pagination_helper:read_collection(Arg,
                                         Options,
                                         fun internal_process_orgs/1).

list_user_organizations(User, Options) when is_list(User) and is_list(Options) ->
  Url = octo_url_helper:generate_url(user_organizations, [User], Options),
  octo_http_helper:read_collection(Url, Options, fun internal_process_orgs/1);
list_user_organizations(Arg, Options) when is_tuple(Arg) and is_list(Options) ->
  octo_pagination_helper:read_collection(Arg,
                                         Options,
                                         fun internal_process_orgs/1).

read_organization(Organization, Options) ->
  Url = octo_url_helper:generate_url(organization, [Organization], Options),
  octo_http_helper:read_to_record(
    Url, Options, fun(Json) -> ?json_to_record(octo_organization, Json) end).

update_organization(Organization, Payload, Options) ->
  Url = octo_url_helper:generate_url(organization, [Organization], Options),
  PayloadJson  = jsonerl:encode(Payload),
  case octo_http_helper:patch(Url, Options, PayloadJson) of
    {ok, Result} -> {ok, ?json_to_record(octo_organization, Result)};
    Other -> Other
  end.

%% Internal functions

internal_process_orgs(Organizations) ->
  [ ?struct_to_record(octo_organization, Org) || Org <- Organizations].
