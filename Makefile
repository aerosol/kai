.PHONY: \
	all \
	compile \
	compile-fast \
	deps \
	eunit \
	test \

all: deps compile test

deps:
	@rebar get-deps

console: compile-fast
	@erl -pa ebin -pa deps/*/ebin -s kai -boot start_sasl -sname kai_dev

compile:
	@rebar compile

compile-fast:
	@rebar compile skip_deps=true

test: ct

ct:
	@rebar ct skip_deps=true

dialyze:
	@dialyzer -r --fullpath -Wno_undefined_callbacks -r ./ebin -r ./deps/*/ebin
