# octo.erl [![Build Status](https://travis-ci.org/sdepold/octo.erl.svg?branch=master)](https://travis-ci.org/sdepold/octo.erl) [![Coverage Status](https://coveralls.io/repos/github/sdepold/octo.erl/badge.svg?branch=master)](https://coveralls.io/github/sdepold/octo.erl?branch=master)

The library `octo.erl` is a wrapper for the Github API written in Erlang.
It focuses on reading data. Write ability might be added later.

## Installation

You can install `octo.erl` via rebar. Just add it as dependency to your `rebar.config`:

```erlang
{deps, [
  {octo, ".*", {git, "git://github.com/sdepold/octo.erl.git", {branch, "master"}}}
]}.
```

Afterwards you can download and compile it:

```
rebar get-deps compile
```

## Usage

### Application

Even though it's a library, it has a state to keep. Thus, you must start its
application before calling any of the library's functions:

```erlang
application:start(octo).
```

### Records

This lib ships a couple of record definitions which can be loaded via the `include_lib` directive:

```erlang
-include_lib("octo/include/octo.hrl").
```

Or in the Erlang shell:

```erlang
rr("deps/octo/include/octo.hrl").
```

Any function can return `octo_error` record containing `message` and
`documentation_url` fields describing the error. Other than that, functions
return records that are specific to their purpose; for example,
`update_pull_request` will return `octo_pull_request`.

### Authentication

At te moment, octo.erl only supports authentication using OAuth2 Token (sent in
a header). To set it, run `octo:set_credentials(pat, "d34dbeef")`.

### API

All the following commands return tuples à la:

```erlang
{ok, Result}
```

#### Pull Request

```erlang
%% Reading a specific pull request of a repository.
%%
%% Args:
%% - Owner:  Owner of the repository. Char list.
%% - Repo:   Name of the repository. Char list.
%% - Number: The (repo scoped) number of the pull request. Integer.
%%
%% Returns an octo_pull_request record.
octo:read_pull_request(Owner, Repo, Number).

%% Reading all pull request of a repository.
%%
%% Args:
%% - Owner: Owner of the repository. Char list.
%% - Repo:  Name of the repository. Char list.
%%
%% Returns a list of octo_pull_request records.
octo:list_pull_requests(Owner, Repo).

%% Reading a pull request's commits.
%%
%% Args:
%% - Owner:  Owner of the repository. Char list.
%% - Repo:   Name of the repository. Char list.
%% - Number: The (repo scoped) number of the pull request. Integer.
%%
%% Returns an octo_commit record.
octo:list_pull_request_commits(Owner, Repo, Number).

%% Reading a pull request's files.
%%
%% Args:
%% - Owner:  Owner of the repository. Char list.
%% - Repo:   Name of the repository. Char list.
%% - Number: The (repo scoped) number of the pull request. Integer.
%%
%% Returns a list of octo_file records.
octo:list_pull_request_files(Owner, Repo, Number).

%% Determing whether a pull request is merged.
%%
%% Args:
%% - Owner:  Owner of the repository. Char list.
%% - Repo:   Name of the repository. Char list.
%% - Number: The (repo scoped) number of the pull request. Integer.
%%
%% Returns a whether or not a pull request is merged.
octo:is_pull_request_merged(Owner, Repo, Number).

%% Creating a pull request in a repository.
%%
%% Title, Head and Base can be either lists of chars in UTF-8, or binaries.
%% It's acceptable to mix types (e.g. pass Title as binary and two others as
%% lists, or vice versa.)
%%
%% Returns the just created pull request.
octo:create_pull_request(Owner, Repo, Title, Head, Base).

%% Update a pull request.
%%
%% Currently useless as it doesn't give us a way to actually update anything.
%%
%% Returns the updated pull request.
octo:update_pull_request(Owner, Repo, Number).

%% Merge a pull request.
%%
%% Args:
%% - Owner:  Owner of the repository. Char list.
%% - Repo:   Name of the repository. Char list.
%% - Number: The (repo scoped) number of the pull request. Integer.
octo:merge_pull_request(Owner, Repo, Number).
```

#### References

```erlang
%% List all references of a repository.
%%
%% Args
%% - Owner: Owner of the repository. Char list.
%% - Repo:  Name of the repository. Char list.
%%
%% Returns a list of octo_references.
octo:list_references(Owner, Repo).

%% List all branches of a repository.
%%
%% Args
%% - Owner: Owner of the repository. Char list.
%% - Repo:  Name of the repository. Char list.
%%
%% Returns a list of branch octo_references.
octo:list_branches(Owner, Repo).

%% List all tags of a repository.
%%
%% Args
%% - Owner: Owner of the repository. Char list.
%% - Repo:  Name of the repository. Char list.
%%
%% Returns a list of tag octo_references.
octo:list_tags(Owner, Repo).

%% Read a specific reference of a repository.
%%
%% Args
%% - Owner:   Owner of the repository. Char list.
%% - Repo:    Name of the repository. Char list.
%% - RefName: The to be read reference. Char list.
%%     Valid examples:
%%     - refs/heads/my-branch
%%     - heads/my-branch
%%     - tags/my-tag
%%
%% Returns a list of tag octo_references.
octo:read_reference(Owner, Repo, RefName).

%% Read a specific branch of a repository.
%%
%% Args
%% - Owner:      Owner of the repository. Char list.
%% - Repo:       Name of the repository. Char list.
%% - BranchName: The to be read branch. Char list.
%%     Valid examples:
%%     - my-branch
%%     Invalid examples:
%%     - heads/my-branch
%%     - refs/heads/my-branch
%%
%% Returns a branch instance of octo_reference.
octo:read_branch(Owner, Repo, BranchName).

%% Read a specific tag of a repository.
%%
%% Args
%% - Owner:   Owner of the repository. Char list.
%% - Repo:    Name of the repository. Char list.
%% - TagName: The to be read tag. Char list.
%%     Valid examples:
%%     - v1.2.3
%%     Invalid examples:
%%     - tags/v1.2.3
%%     - refs/tags/v1.2.3
%%
%% Returns a tag instance of octo_reference.
octo:read_tag(Owner, Repo, TagName).

%% Creating a reference in a repository.
%%
%% Args:
%% - Owner:   Owner of the repository. Char list.
%% - Repo:    Name of the repository. Char list.
%% - Ref:     The name of a fully qualified reference. Char list or binary.
%% - Sha:     The SHA1 value to set this reference to. Char list or binary.
%%
%% Returns the just created octo_reference.
octo:create_reference(Owner, Repo, Ref, Sha).

%% Creating a branch in a repository.
%%
%% Args:
%% - Owner:      Owner of the repository. Char list.
%% - Repo:       Name of the repository. Char list.
%% - BranchName: Name of the to be created branch. Char list.
%% - Source:     The sha from which to create the branch from. Char list.
%%
%% Returns the just created branch octo_reference.
octo:create_branch(Owner, Repo, BranchName, Source).

%% Creating a tag in a repository.
%%
%% Args:
%% - Owner:   Owner of the repository. Char list.
%% - Repo:    Name of the repository. Char list.
%% - TagName: Name of the to be created tag. Char list.
%% - Source:  The sha from which to create the tag from. Char list.
%%
%% Returns the just created branch octo_reference.
octo:create_tag(Owner, Repo, TagName, Source).

%% Updating a reference in a repository.
%%
%% Args:
%% - Owner:   Owner of the repository. Char list.
%% - Repo:    Name of the repository. Char list.
%% - RefName: Name of the to be created tag. Char list.
%%     - Valid examples:   refs/heads/my-branch, heads/my-branch, refs/tags/my-tag, tags/my-tag
%%     - Invalid examples: my-branch, my-tag
%% - Payload: Meta information about the to be updated reference. Tuple of tuples.
%%     Example:
%%     {
%%       {<<"sha">>, <<"aa218f56b14c9653891f9e74264a383fa43fefbd">>},
%%       {<<"force">>, true|false}
%%     }
%%
%% Returns the just updated branch octo_reference.
octo:update_reference(Owner, Repo, RefName, Payload).

%% Delete a specific reference of a repository.
%%
%% Args
%% - Owner:   Owner of the repository. Char list.
%% - Repo:    Name of the repository. Char list.
%% - RefName: Name of the to be deleted reference. Char list.
%%     Valid examples:
%%     - refs/heads/my-branch
%%     - heads/my-branch
%%     - tags/my-tag
%%
%% Returns {ok, null}.
octo:delete_reference(Owner, Repo, RefName).

%% Delete a specific branch of a repository.
%%
%% Args
%% - Owner:      Owner of the repository. Char list.
%% - Repo:       Name of the repository. Char list.
%% - BranchName: Name of the to be deleted branch. Char list.
%%     Valid examples:
%%     - my-branch
%%
%% Returns {ok, null}.
octo:delete_branch(Owner, Repo, BranchName).

%% Delete a specific tag of a repository.
%%
%% Args
%% - Owner:   Owner of the repository. Char list.
%% - Repo:    Name of the repository. Char list.
%% - TagName: Name of the to be deleted tag. Char list.
%%     Valid examples:
%%     - my-tag
%%
%% Returns {ok, null}.
octo:delete_tag(Owner, Repo, TagName).
```

### Organization

```erlang
%% List organizations you have some access to (e.g. read something from).
%%
%% Args:
%% - Options: see Options section below.
%%
%% Returns: {ok, [#octo_organization{}]}
octo:list_my_organizations(Options).

%% List organizations user's the member of.
%%
%% Args:
%% - User: Username of the user whose organizations we want to list. Char list.
%% - Options: see Options section below.
%%
%% Returns: {ok, [#octo_organization{}]}
octo:list_user_organizations(User, Options).

%% Retrieve all the info about an organization.
%%
%% Args:
%% - Organization: The name of the organization to retrieve. Char list.
%% - Options: see Options section below.
%%
%% Returns: {ok, #octo_organization{}}
octo:read_organization(Organization, Options).

%% Update some of an organization's details.
%%
%% Args:
%% - Organization: The name of the organization to update. Char list.
%% - Payload: Fields to update. Tuple of tuples.
%%     Example:
%%     {
%%       {<<"billing_email">>, <<"will.not.be.publicized@example.com">>},
%%       {<<"company">>,       <<"Stealth Mode">>},
%%       {<<"email">>,         <<"will.be.publicly.listed@example.com">>},
%%       {<<"location">>,      <<"Best place on Earth">>},
%%       {<<"name">>,          <<"cool_org">>},
%%       {<<"description">>,   <<"Coolest org evar">>},
%%     }
%% - Options: see Options section below.
%%
%% Returns: an updated #octo_organization().
octo:update_organization(Organization, Payload, Options).
```

### Pagination

GitHub API [paginates](https://en.wikipedia.org/wiki/Pagination) its responses
wherever possible. octo.erl can either fetch all the pages for you in one call
(effectively hiding pagination from you), or it can let you handle it yourself.

To make octo.erl fetch all the pages, read about `all_pages` option below.

If you decided to consume pages one by one, here's how you do it:

1. make an ordinary request:

   ```erlang
   {ok, Page1} = octo:list_whatever(you, want).
   ```

2. ask octo.erl to fetch the next page of results:

   ```erlang
   {ok, Page2} = octo:list_whatever({next, Page1}).
   ```

You can request `prev`, `next`, `first` and `last` pages.

Pagination is supported by all of the "list" functions.

There's a caveat, though. The following code will result in a cache miss,
costing you a tiny bit of memory for an unused cache entry and also making
a call to API that would otherwise be avoided.

```erlang
{ok, Page1} = octo:list_whatever("user", "repo"),
{ok, Page2} = octo:list_whatever({next, Page2}),
{ok, Page1Again} = octo:list_whatever({prev, Page2}).
```

This only affects the first page of the results. For details, see [this
comment](https://github.com/sdepold/octo.erl/issues/25#issuecomment-198951078)
in our issue tracker.

### Options

All of the aforementioned functions can be called with an additional options
parameter. It has to be a proplist (a list containing `{key, value}` pairs;
`{key, true}` is equivalent to `key`). The following options are supported:

#### Number of results per page

By default, GitHub returns 30 results per page. To change that to, say, 100
results per page, do the following:

```erlang
Options            = [{per_page, 100}],
{ok, PullRequests} = octo:read_pull_request(Owner, Repo, Number, Options).
```

#### Reading all entries

If you don't want to do the pagination manually, you can provide an option that
resolves all pages automatically:

```erlang
Options            = [all_pages],
{ok, PullRequests} = octo:read_pull_request(Owner, Repo, Number, Options).
```

## Implementation state

See [our implementation state
milestone](https://github.com/sdepold/octo.erl/milestone/1).

## Development notes

`octo.erl` uses rebar as build tool. You can install it on OS X like this:

```
brew install rebar
```

You can compile the code like this:

```
git clone git@github.com:sdepold/octo.erl.git
cd octo.erl
make
```

### Running the tests

Unit tests can be run at any time with the following command:

```
make tests
```

You'll see a lot of info reports from SASL about `octo` application being
stopped—don't panic, this is an expected behaviour. Look for a line saying "All
N tests passed" or check the exit code (should be zero).

Integration tests are a bit more involved:

1. (Optionally) Create a dedicated login for testing.

   While it's not strictly necessary, separate login minimises the chances of
   breaking anything important.

2. Clone `sdepold/octo.erl-test` repo (under the testing login).

3. Set `TESTING_LOGIN` environment variable.

4. Generate an authentication token (for the testing account) and set
   `AUTH_TOKEN` environment variable. It's sufficient to give it access to your
   repos, as we don't support anything else at the moment anyway.

5. Now, you can run tests with:
   ```
   make integration-tests
   ```
