APPLICATION := getopt

REBAR=$(shell which rebar || echo ./rebar)
ERL := erl
EPATH := -pa ebin
TEST_EPATH := -pa .eunit

DIALYZER=dialyzer
DIALYZER_OPTS=-Wno_return -Wrace_conditions -Wunderspecs -Wno_undefined_callbacks --fullpath

.PHONY: all clean compile console dialyze doc test test-console

all: compile

clean:
	@$(REBAR) clean

compile:
	@$(REBAR) compile

console:
	$(ERL) -sname $(APPLICATION) $(EPATH)

dialyze: compile
	@$(DIALYZER) $(DIALYZER_OPTS) -r ./

doc:
	@$(REBAR) doc

test:
	@$(REBAR) eunit

test-console:
	$(ERL) -sname $(APPLICATION)_test $(TEST_EPATH)

