APPLICATION := getopt

.PHONY: all clean compile dialyzer edoc shell test

all: compile

clean:
	@rebar3 clean

compile:
	@rebar3 compile

dialyzer: compile
	@rebar3 dialyzer

edoc:
	@rebar3 edoc

shell:
	@rebar3 shell

test:
	@rebar3 eunit

