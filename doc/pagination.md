# Pagination

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
