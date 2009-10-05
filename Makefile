PROJECT=getopt
ERL=erl
ERLC=erlc -I include -v -o ebin
SOURCES=src/*.erl
EPATH=-pa ebin
DOC_OPTS={dir, \"doc\"}, {source_path, [\"include\", \"src\"]}

all:
	@mkdir -p ebin
	$(ERL) $(EPATH) -make

run:
	$(ERL) -sname "$(PROJECT)" $(EPATH)

test: all
	$(ERL) -noshell $(EPATH) -s $(PROJECT)_test test -s init stop

edoc: all
	$(ERL) -noshell $(EPATH) \
		-eval "edoc:files([string:tokens(\"src/getopt.erl\", \" \")], [$(DOC_OPTS)])" \
		-s init stop


clean:
	rm -fv ebin/*.beam
	rm -fv doc/*
	rm -fv erl_crash.dump ebin/erl_crash.dump
