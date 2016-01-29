all: app

deps:
	rebar get-deps
	rebar compile

app: deps
	rebar compile

tests:
	rebar eunit

integration-tests: app
	rebar ct

clean:
	rebar clean

distclean: clean
	rebar delete-deps

.PHONY: all deps app tests clean distclean
