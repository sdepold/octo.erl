# Options

All of the aforementioned functions can be called with an additional options
parameter. It has to be a proplist (a list containing `{key, value}` pairs;
`{key, true}` is equivalent to `key`). The following options are supported:

## Number of results per page

By default, GitHub returns 30 results per page. To change that to, say, 100
results per page, do the following:

```erlang
Options            = [{per_page, 100}],
{ok, PullRequests} = octo:read_pull_request(Owner, Repo, Number, Options).
```

## Reading all entries

If you don't want to do the pagination manually, you can provide an option that
resolves all pages automatically:

```erlang
Options            = [all_pages],
{ok, PullRequests} = octo:read_pull_request(Owner, Repo, Number, Options).
```
