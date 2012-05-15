APPLICATION := getopt

REBAR=$(shell which rebar || echo ./rebar)
ERL := erl
EPATH := -pa ebin
TEST_EPATH := -pa .eunit

DIALYZER=dialyzer
DIALYZER_OPTS=-Wno_return -Wrace_conditions -Wunderspecs -Wbehaviours
PLT_FILE=.getopt_plt
APPS=kernel stdlib

.PHONY: all clean test

all: compile

compile:
	@$(REBAR) compile

doc:
	@$(REBAR) doc

plt: compile
	@$(DIALYZER) --build_plt --output_plt $(PLT_FILE) --apps $(APPS) ebin

check_plt: compile
	@$(DIALYZER) --check_plt --plt $(PLT_FILE) --apps $(APPS) ebin

analyze: compile
	@$(DIALYZER) --plt $(PLT_FILE) $(DIALYZER_OPTS) -r ebin

clean:
	@$(REBAR) clean

test:
	@$(REBAR) eunit

dialyzer:
	@$(REBAR) analyze

console: compile
	$(ERL) -sname $(APPLICATION) $(EPATH)

testshell: test
	$(ERL) -sname $(APPLICATION)_test $(TEST_EPATH)

