Getopt for Erlang
=================

Command-line parsing module that uses a syntax similar to that of GNU *getopt*.


Requirements
------------

You should only need a somewhat recent version of Erlang/OTP, though the module
has only been tested with Erlang R13B.


Installation
------------

To compile the module you simply run ``make``.

To run the unit tests run ``make test``.

To run the example module run ``make example``.

To build the (very) limited documentation run ``make doc``.


Usage
-----

The *getopt* module provides four functions:

    parse([{Name, Short, Long, ArgSpec, Help}], Args :: string() | [string()]) ->
        {ok, {Options, NonOptionArgs}} | {error, {Reason, Data}}

    usage([{Name, Short, Long, ArgSpec, Help}], ProgramName :: string()) -> ok

    usage([{Name, Short, Long, ArgSpec, Help}], ProgramName :: string(),
          CmdLineTail :: string()) -> ok

    usage([{Name, Short, Long, ArgSpec, Help}], ProgramName :: string(),
          CmdLineTail :: string(), OptionsTail :: [{string(), string}]) -> ok

The ``parse/2`` function receives a list of tuples with the command line option
specifications. The type specification for the tuple is:

    -type arg_type() :: 'atom' | 'binary' | 'boolean' | 'float' | 'integer' | 'string'.

    -type arg_value() :: atom() | binary() | boolean() | float() | integer() | string().

    -type arg_spec() :: arg_type() | {arg_type(), arg_value()} | undefined.

    -type option_spec() :: {
                       Name    :: atom(),
                       Short   :: char() | undefined,
                       Long    :: string() | undefined,
                       ArgSpec :: arg_spec(),
                       Help    :: string() | undefined
                      }.

The elements of the tuple are:

  - ``Name``: name of the option.
  - ``Short``: character for the short option (e.g. $i for -i).
  - ``Long``: string for the long option (e.g. "info" for --info).
  - ``ArgSpec``: data type and optional default value the argument will be converted to.
  - ``Help``: help message that is shown for the option when ``usage/2`` is called.

e.g.

     {port, $p, "port", {integer, 5432}, "Database server port"}

The second parameter receives the list of arguments as passed to the ``main/1``
function in escripts or the unparsed command line as a string.

If the function is successful parsing the command line arguments it will return
a tuple containing the parsed options and the non-option arguments. The options
will be represented by a list of key-value pairs with the ``Name`` of the
option as *key* and the argument from the command line as *value*. If the option
doesn't have an argument, only the atom corresponding to its ``Name`` will be
added to the list of options. For the example given above we could get something
like ``{port, 5432}``. The non-option arguments are just a list of strings with
all the arguments that did not have corresponding options.

e.g. For a program named ``ex.escript`` with the following option specifications:

    OptSpecList =
        [
         {host,    $h,        "host",    {string, "localhost"}, "Database server host"},
         {port,    $p,        "port",    integer,               "Database server port"},
         {dbname,  undefined, "dbname",  {string, "users"},     "Database name"},
         {xml,     $x,        undefined, undefined,             "Output data in XML"},
         {verbose, $v,        "verbose", integer,               "Verbosity level"},
         {file,    undefined, undefined, string,                "Output file"}
        ].

And this command line:

    Args = "-h myhost --port=1000 -x myfile.txt -vvv dummy1 dummy2"

Which could also be passed in the format the ``main/1`` function receives the arguments in escripts:

    Args = ["-h", "myhost", "--port=1000", "-x", "file.txt", "-vvv", "dummy1", "dummy2"].

The call to ``getopt:parse/2``:

    getopt:parse(OptSpecList, Args).

Will return:

    {ok,{[{host,"myhost"},
          {port,1000},
          xml,
          {file,"file.txt"},
          {dbname,"users"},
          {verbose,3}],
         ["dummy1","dummy2"]}}


The other functions exported by the ``getopt`` module (``usage/2``, ``usage/3``
and ``usage/4``) are used to show the command line syntax for the program.
For example, given the above-mentioned option specifications, the call to
``getopt:usage/2``:

    getopt:usage(OptSpecList, "ex1").

Will show (on *stderr*):

    Usage: ex1 [-h <host>] [-p <port>] [--dbname <dbname>] [-x] [-v] <file>

      -h, --host                    Database server host
      -p, --port                    Database server port
      --dbname                      Database name
      -x                            Output data in XML
      -v                            Verbosity level
      <file>                        Output file

This call to ``getopt:usage/3`` will add a string after the usage command line:

    getopt:usage(OptSpecList, "ex1", "[var=value ...] [command ...]").

Will show (on *stderr*):

    Usage: ex1 [-h <host>] [-p <port>] [--dbname <dbname>] [-x] [-v <verbose>] <file> [var=value ...] [command ...]

      -h, --host            Database server host
      -p, --port            Database server port
      --dbname              Database name
      -x                    Output data in XML
      -v, --verbose         Verbosity level
      <file>                Output file

Whereas this call to ``getopt:usage/3`` will also add some lines to the options
help text:

    getopt:usage(OptSpecList, "ex1", "[var=value ...] [command ...]",
                 [{"var=value", "Variables that will affect the execution (e.g. debug=1)"},
                  {"command",   "Commands that will be executed (e.g. count)"}]).

Will show (on *stdout*):

    Usage: ex1 [-h <host>] [-p <port>] [--dbname <dbname>] [-x] [-v <verbose>] <file> [var=value ...] [command ...]

      -h, --host            Database server host
      -p, --port            Database server port
      --dbname              Database name
      -x                    Output data in XML
      -v, --verbose         Verbosity level
      <file>                Output file
      var=value             Variables that will affect the execution (e.g. debug=1)
      command               Commands that will be executed (e.g. count)


Command-line Syntax
-------------------

The syntax supported by the ``getopt`` module is very similar to that followed
by GNU programs, which is described [here](http://www.gnu.org/s/libc/manual/html_node/Argument-Syntax.html).

Options can have both short (single character) and long (string) option names.

A short option can have the following syntax:

    -a         Single option 'a', no argument or implicit boolean argument
    -a foo     Single option 'a', argument "foo"
    -afoo      Single option 'a', argument "foo"
    -abc       Multiple options: 'a'; 'b'; 'c'
    -bcafoo    Multiple options: 'b'; 'c'; 'a' with argument "foo"
    -aaa       Multiple repetitions of option 'a' (when 'a' has integer arguments)

A long option can have the following syntax:

    --foo      Single option 'foo', no argument
    --foo=bar  Single option 'foo', argument "bar"
    --foo bar  Single option 'foo', argument "bar"

We can also have options with neither short nor long option name. In this case,
the options will be taken according to their position in the option specification
list passed to ``getopt:/parse2``.

For example, with the following option specifications:

    OptSpecList =
        [
         {xml,         $x,        "xml",         undefined,             "Output data as XML"},
         {dbname,      undefined, undefined,     string,                "Database name"},
         {output_file, undefined, undefined,     string,                "File where the data will be saved to"}
        ].

And these arguments:

    Args = "-x mydb file.out dummy1 dummy1".

The call to ``getopt:parse/2``:

    getopt:parse(OptSpecList, Args).

Will return:

    {ok,{[xml,{dbname,"mydb"},{output_file,"file.out"}],
         ["dummy1","dummy1"]}}

Finally, the string ``--`` is considered an option terminator (i.e. all
arguments after it are considered non-option arguments) and the single ``-``
character is considered as non-option argument too.


Argument Types
--------------

The arguments allowed for options are: *atom*; *binary*; *boolean*; *float*; *integer*; *string*.
The ``getopt`` module checks every argument to see if it can be converted to its
correct type. In the case of boolean arguments, the following values (in lower or
upper case) are considered ``true``: *true*; *t*; *yes*; *y*; *on*; *enabled*; *1*.

And these ones are considered ``false``: *false*; *f*; *no*; *n*; *off*; *disabled*; *0*.
