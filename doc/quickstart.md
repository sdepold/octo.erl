# Quickstart Guide

1. Include `octo.hrl` to get access to our records:

```erlang
-include_lib("octo/include/octo.hrl").
```

2. Start `octo` application:

```erlang
application:start(octo).
```

3. If you're going to fire more then few requests per hour, you better
   authenticate:

```erlang
octo:set_credentials(pat, "d34dbeef")
```

4. Read our [function naming convention](naming.md) and go search our [API
   docs](https://sdepold.github.io/octo.erl/) for functions that you need.
