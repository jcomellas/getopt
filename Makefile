APPLICATION := getopt

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
	@rebar compile

doc:
	@rebar doc

plt: compile
	@$(DIALYZER) --build_plt --output_plt $(PLT_FILE) --apps $(APPS) ebin

check_plt: compile
	@$(DIALYZER) --check_plt --plt $(PLT_FILE) --apps $(APPS) ebin

analyze: compile
	@$(DIALYZER) --plt $(PLT_FILE) $(DIALYZER_OPTS) -r ebin

clean:
	@rebar clean

test:
	@rebar eunit

dialyzer:
	@rebar analyze

console: compile
	$(ERL) -sname $(APPLICATION) $(EPATH)

testshell: test
	$(ERL) -sname $(APPLICATION)_test $(TEST_EPATH)

