# octo.erl [![Build Status](https://travis-ci.org/sdepold/octo.erl.svg?branch=feature%2Flist-pull-requests)](https://travis-ci.org/sdepold/octo.erl)

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

### Records

This lib ships a couple of record definitions which can be loaded via the `include_lib` directive:

```erlang
-include_lib("octo/include/octo.hrl").
```

Or in the Erlang shell:

```erlang
rr("deps/octo/include/octo.hrl").
```

#### octo_pull_request

To do: Explain octo_pull_request.

#### octo_commit

To do: Explain octo_commit.

#### octo_file

To do: Explain octo_file.

#### octo_reference

To do: Explain octo_reference.

### API

Every of the following commands returns a tuple à la:

```erlang
{ok, Result}
```

The following paragraphs are describing the existing functions.

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
%% Args:
%% - Owner:   Owner of the repository. Char list.
%% - Repo:    Name of the repository. Char list.
%% - Payload: Meta information about the to be created PR. Tuple of tuples.
%%     Example:
%%     {
%%       {<<"title">>, <<"Pull request title">>},
%%       {<<"body">>, <<"Pull request description">>},
%%       {<<"head">>, <<"Head branch">>},
%%       {<<"base">>, <<"Base branch">>}
%%     }
%%
%% Returns the just created pull request.
octo:create_pull_request(Owner, Repo, Payload).

%% Update a pull request.
%%
%% Args:
%% - Owner:   Owner of the repository. Char list.
%% - Repo:    Name of the repository. Char list.
%% - Number:  The (repo scoped) number of the pull request. Integer.
%% - Payload: Meta information about the update. Tuple of tuples.
%%     Example:
%%     {
%%       {<<"state">>, <<"closed">>},
%%       {<<"title">>, <<"Another title">>},
%%       {<<"body">>, <<"Another body">>}
%%     }
%%
%% Returns the updated pull request.
octo:update_pull_request(Owner, Repo, Number, Payload).

%% Update a pull request.
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
%% - Payload: Meta information about the to be created reference. Tuple of tuples.
%%     Example:
%%     {
%%       {<<"ref">>, <<"refs/heads/featureA">>},
%%       {<<"sha">>, <<"aa218f56b14c9653891f9e74264a383fa43fefbd">>}
%%     }
%%
%% Returns the just created octo_reference.
octo:create_reference(Owner, Repo, Payload).

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

### Options

Every of the mentioned functions can be called with an additional options parameter. It has to be
a list. The following options are available:

#### Authentication

Currently it is only possible to authenticate via a *Personal Api token*. You can find further
information about this topic here: https://github.com/blog/1509-personal-api-tokens

```erlang
Options           = [{ auth, pat, "your_personal_api_token" }].
{ok, PullRequest} = octo:read_pull_request(Owner, Repo, Number, Options).
```

#### Pagination

Every function that returns a list is paginated. By default that are a 30 entries per page.
You can configure the pagination like this:

```erlang
Options            = [{ page, 1 }, { per_page, 100 }].
{ok, PullRequests} = octo:read_pull_request(Owner, Repo, Number, Options).
```

#### Reading all entries

If you don't want to do the pagination manually, you can also provide an option that resolves all
pages automatically:

```erlang
Options            = [{ all_pages }].
{ok, PullRequests} = octo:read_pull_request(Owner, Repo, Number, Options).
```

## Implementation state

- [ ] Activity
- [ ] Gists
  - [ ] List gists
  - [ ] Get a single gist
  - [ ] Create a gist
  - [ ] Edit a gist
  - [ ] List gist commits
  - [ ] Star a gist
  - [ ] Unstar a gist
  - [ ] Check if a gist is starred
  - [ ] Fork a gist
  - [ ] List gist forks
  - [ ] Delete a gist
- [ ] Git Data
  - [ ] Blobs
    - [ ] Get a Blob
    - [ ] Create a Blob
    - [ ] Custom media types
  - [ ] Commits
    - [ ] Get a Commit
    - [ ] Create a Commit
  - [x] References
    - [x] Get a Reference
    - [x] Get all References
    - [x] Create a Reference
    - [x] Update a Reference
    - [x] Delete a Reference
  - [ ] Tags
    - [ ] Get a Tag
    - [ ] Create a Tag Object
  - [ ] Trees
    - [ ] Get a Tree
    - [ ] Get a Tree Recursively
    - [ ] Create a Tree
- [ ] Issues
  - [ ] List issues
  - [ ] List issues for a repository
  - [ ] Get a single issue
  - [ ] Create an issue
  - [ ] Edit an issue
- [ ] Misc
- [ ] Organizations
  - [ ] List your organizations
  - [ ] List user organizations
  - [ ] Get an organization
  - [ ] Edit an organization
- [ ] Pull Requests
  - [x] List pull requests
  - [x] Get a single pull request
  - [x] Create a pull request
  - [x] Update a pull request
  - [x] List commits on a pull request
  - [x] List pull requests files
  - [x] Get if a pull request has been merged
  - [x] Merge a pull request (Merge Button)
- [ ] Repositories
  - [ ] List your repositories
  - [ ] List user repositories
  - [ ] List organization repositories
  - [ ] List all public repositories
  - [ ] Create
  - [ ] Get
  - [ ] Edit
  - [ ] List contributors
  - [ ] List languages
  - [ ] List Teams
  - [ ] List Tags
  - [ ] List Branches
  - [ ] Get Branch
  - [ ] Delete a Repository
- [ ] Search
  - [ ] Search repositories
  - [ ] Search code
  - [ ] Search issues
  - [ ] Search users
  - [ ] Text match metadata
- [ ] Users
  - [ ] Get a single user
  - [ ] Get the authenticated user
  - [ ] Update the authenticated user
  - [ ] Get all users
- [ ] Enterprise

## Development notes

`octo.erl` uses rebar as build tool. You can install it on OS X like this:

```
brew install rebar
```

You can compile the code and run the tests like this:

```
git clone git@github.com:sdepold/octo.erl.git
cd octo.erl
rebar get-deps compile
```

### Running the tests

When running the tests you might run into rate limits. You can improve the situation via providing
an authentication token (PAT) like this:

```
AUTH_TOKEN="YOUR_TOKEN" rebar eunit
```
