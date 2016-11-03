# Records

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
