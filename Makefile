all: app

deps:
	rebar prepare-deps

app: deps
	rebar compile skip_deps=true

tests: app
	rebar eunit

integration-tests: app
	rebar ct

clean:
	rebar clean

distclean: clean
	rebar delete-deps

.PHONY: all deps app tests clean distclean
