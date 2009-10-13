PROJECT=getopt
ERL=erl
ERLC=erlc -I include -v -o ebin
SOURCES=src/*.erl
EPATH=-pa ebin
DOC_OPTS={dir, \"doc\"}, {includes, [\"include\"]}, {source_path, [\"include\", \"src\"]}

all:
	@mkdir -p ebin
	@$(ERL) $(EPATH) -make

run: all
	$(ERL) -sname "$(PROJECT)" $(EPATH)

test: all
	@$(ERL) -noshell $(EPATH) -s $(PROJECT)_test test -s init stop

example: all
	@$(ERL) -noshell $(EPATH) -s ex1 test -s init stop

docs: all
	@$(ERL) -noshell $(EPATH) \
		-eval "edoc:files(filelib:wildcard(\"$(SOURCES)\"), [$(DOC_OPTS)])" \
		-s init stop

clean:
	rm -fv ebin/*.beam
	rm -fv erl_crash.dump ebin/erl_crash.dump

distclean:
	rm -fv ebin/*.beam
	rm -fv doc/*
	rm -fv erl_crash.dump ebin/erl_crash.dump

docclean:
	rm -fv doc/*
