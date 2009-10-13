Getopt for Erlang
=================

Command-line parsing module that uses a syntax similar to that of GNU Getopt.


Requirements
------------

You should only need a somewhat recent version of Erlang/OTP, though the module
has only been tested with Erlang R13B.


Installation
------------

To compile the module you simply run 'make'.

To run the unit tests run 'make test'.

To run the example module run 'make example'.

To build the (very) limited documentation run 'make docs'.


Usage
-----

The *getopt* module provides two functions:

    parse([#option{}], Args :: string() | [string()]) -> {ok, {Options, NonOptionArgs}} | {error, {Reason, Data}}
    usage([#option{}], ProgramName :: string()) -> ok

The ``parse/2`` function receives a list of ``option`` records (defined in
``getopt.hrl``) with the command line option specifications. The ``option``
record has the following elements:

    -record(option, {
              name              :: atom(),
              short             :: char() | undefined,
              long              :: string() | undefined,
              arg               :: getopt_arg_type() | {getopt_arg_type(), getopt_arg()} | undefined.
              help              :: string() | undefined
             }).

The fields of the record are:

  - ``name``: name of the option.
  - ``short``: character for the short option (e.g. $i for -i).
  - ``long``: string for the long option (e.g. "info" for --info).
  - ``arg``: data type the argument will be converted to with an optional
             default value. It can either be an atom() (one of: 'atom',
             'binary', 'boolean', 'float', 'integer', 'string') or a tuple with
             an atom() and the default value for that argument.
  - ``help``: help message that is shown for the option when usage/2 is called.

The second parameter holds the list of arguments as passed to the ``main/1``
function in escripts.

e.g.

     #option{name     = port,
             short    = $p,
             long     = "port",
             arg      = {integer, 5432},
             help     = "Database server port"
            }

If the function is successful parsing the command line arguments it will return
a tuple containing the parsed options and the non-option arguments. The options
will be represented by a list of key-value pairs with the ``name`` of the
option as *key* and the argument from the command line as *value*. If the option
doesn't have an argument, only the atom corresponding to its ``name`` will be
added to the list of options. For the example given above we could get something
like ``{port, 5432}``. The non-option arguments are just a list of strings with
all the arguments that did not have corresponding options.

e.g. For a program named ex1.escript with the following option specifications:

    OptSpec =
        [
         #option{name     = host,
                 short    = $h,
                 long     = "host",
                 arg      = {string, "localhost"},
                 help     = "Database server host"
                },
         #option{name     = port,
                 short    = $p,
                 long     = "port",
                 arg      = integer,
                 help     = "Database server port"
                },
         #option{name     = dbname,
                 long     = "dbname",
                 arg      = {string, "users"},
                 help     = "Database name"
                },
         #option{name     = xml,
                 short    = $x,
                 help     = "Output data in XML"
                },
         #option{name     = file,
                 arg      = string,
                 help     = "Output file"
                }
        ].

And this command line:

    Args = "-h myhost --port 1000 -x myfile.txt dummy1 dummy2"

Which could also be passed in the format the ``main/1`` function receives the arguments in escripts:

    Args = ["-h", "myhost", "--port", "1000", "-x", "myfile.txt", "dummy1", "dummy2"].

The call to ``getopt:parse/2``:

    > getopt:parse(OptSpec, Args).

Will return:

    {ok,{[{host,"myhost"},
          {port,1000},
          xml,
          {file,"myfile.txt"},
          {dbname,"users"}],
         ["dummy1","dummy2"]}}

Also, the call to ``getopt:usage/2``:

    > getopt:usage(OptSpec, "ex1").

Will show (on *stdout*):

    Usage: ex1 [-h <host>] [-p <port>] [--dbname <dbname>] [-x] <file>

      -h, --host                    Database server host
      -p, --port                    Database server port
      --dbname                      Database name
      -x                            Output data in XML
      <file>                        Output file


Known limitations
-----------------

  - The syntax for non-option arguments that start with '-' (e.g. -a -- -b)
    is not supported yet.


Known problems
--------------

Currently, with Erlang R13B (and older versions), escripts cannot use the
``-include()`` preprocessor directive so you're forced to install the *getopt*
package in Erlang's library path for your escripts to be able to include the
``getopt.hrl`` header file that defines the ``#option{}`` record used by the
functions in the *getopt* module.
