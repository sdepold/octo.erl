# octo.erl [![Build Status](https://travis-ci.org/sdepold/octo.erl.svg?branch=master)](https://travis-ci.org/sdepold/octo.erl) [![Coverage Status](https://coveralls.io/repos/github/sdepold/octo.erl/badge.svg?branch=master)](https://coveralls.io/github/sdepold/octo.erl?branch=master)

`octo.erl` is a wrapper for the Github API written in Erlang. It isn't
feature-complete yet; see [this
milestone](https://github.com/sdepold/octo.erl/milestone/1) for details.

## Installation

Add a dependency to your `rebar.config`:

```erlang
{deps, [
  {octo, ".*", {git, "git://github.com/sdepold/octo.erl.git", {branch, "master"}}}
]}.
```

Download and compile:

```
rebar get-deps compile
```

## Usage

Start with our [quickstart guide](doc/quickstart.md) and consult the rest of
[the docs](doc/README.md) whenever you need it. Most important of all, have fun!
