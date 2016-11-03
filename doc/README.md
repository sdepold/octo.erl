`octo.erl` provides a thin abstraction over GitHub API. It hides HTTP requests,
(de)serialization, pagination, rate-limiting and caching, and stays out of the
way the rest of the time.

All the functions are in the `octo` module. The [full
list](https://sdepold.github.io/octo.erl/) might be intimidating, but it isn't
meant to be read from beginning to end—just search for whatever operation you
need. To make sense of it all, consult the following documents:

* [Function naming](naming.md)
* [Pagination](pagination.md)
* [General options](options.md)
* [Parameters](parameters.md)





[Quickstart guide](quickstart.md) will get you up to speed in just 4 steps.

Some general topics:

* [Notes for developers](HACKING.md)
