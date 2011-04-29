all:
	rebar get-deps
	rebar compile

clean:
	rebar clean
	rebar delete-deps

test:
	rebar skip_deps=true eunit

dialyzer: compile
	dialyzer -Wno_return -c ebin
