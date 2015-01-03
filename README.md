# octo.erl [![Build Status](https://travis-ci.org/sdepold/octo.erl.svg?branch=feature%2Flist-pull-requests)](https://travis-ci.org/sdepold/octo.erl)

Erlang wrapper for the Github API.

## Development notes

```
git clone git@github.com:sdepold/octo.erl.git
cd octo.erl
wget https://raw.github.com/wiki/rebar/rebar/rebar && chmod u+x rebar
./rebar get-deps compile eunit
```

## Usage

```erlang
octo:list_pull_requests(Username, ProjectName).
```
