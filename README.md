# octo.erl [![Build Status](https://travis-ci.org/sdepold/octo.erl.svg?branch=feature%2Flist-pull-requests)](https://travis-ci.org/sdepold/octo.erl)

The library `octo.erl` is a wrapper for the Github API written in Erlang.
It focuses on reading data. Write ability might be added later.

## Implementation state

- [ ] Activity
- [ ] Gists
- [ ] Git Data
- [ ] Issues
- [ ] Misc
- [ ] Organizations
- [x] Pull Requests
- [ ] Repositories
- [ ] Search
- [ ] Users
- [ ] Enterprise

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

### API

Every of the following commands returns a tuple à la:

```erlang
{ok, Result}
```

This are the available functions:

```erlang
octo:read_pull_request(Username, ProjectName, Number). % Returns a octo_pull_request records.
octo:list_pull_requests(Username, ProjectName). % Returns a list of octo_pull_request records.
octo:list_pull_request_commits(Username, ProjectName, Number). % Returns a list of octo_commit records.
octo:list_pull_request_files(Username, ProjectName, Number). % Returns a list of octo_file records.
```

### Authentication

Every of the mentioned functions can be called with an additional options parameter. It has to be
a list. Supported are the following authentication strategies:

#### Personal API tokens

```erlang
Options           = [{ auth, pat, "your_personal_api_token" }].
{ok, PullRequest} = octo:read_pull_request(Username, ProjectName, Number, Options).
```

You can find further information about this topic here: https://github.com/blog/1509-personal-api-tokens

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
