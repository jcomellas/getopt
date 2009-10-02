PROJECT=getopt
ERL=erl
ERLC=erlc -I include -v -o ebin
SOURCES=src/*.erl
TEST_SOURCES=src/test/*.erl
EPATH=-pa ebin

all:
	@mkdir -p ebin
	$(ERLC) $(SOURCES)
			 
all_test: all
	@mkdir -p ebin
	$(ERLC) -DTEST $(TEST_SOURCES)
		 
run:
	$(ERL) -sname "$(PROJECT)" $(EPATH)
		 
test: all_test
	$(ERL) -noshell $(EPATH) \
		-s $(PROJECT)_test test \
		-s init stop
		 
clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump ebin/erl_crash.dump
