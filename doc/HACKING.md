# Notes for Developers

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

## Unit tests

Unit tests can be run at any time with the following command:

```
make tests
```

You'll see a lot of info reports from SASL about `octo` application being
stopped—don't panic, this is an expected behaviour. Look for a line saying "All
N tests passed" or check the exit code (should be zero).

## Integretion tests

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
