Getopt for Erlang
=================

Command-line parsing module that uses a syntax similar to that of GNU Getopt.


Requirements
------------

You should only need a somewhat recent version of Erlang/OTP, though the module
has only been tested with Erlang R13B.


Installation
------------

To compile the module you simply run ``make``.

To run the unit tests run ``make test``.

To run the example module run ``make example``.

To build the (very) limited documentation run ``make docs``.


Usage
-----

The *getopt* module provides two functions:

    parse([{Name, Short, Long, ArgSpec, Help}], Args :: string() | [string()]) ->
        {ok, {Options, NonOptionArgs}} | {error, {Reason, Data}}

    usage([{Name, Short, Long, ArgSpec, Help}], ProgramName :: string()) -> ok

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

The fields of the record are:

  - ``Name``: name of the option.
  - ``Short``: character for the short option (e.g. $i for -i).
  - ``Long``: string for the long option (e.g. "info" for --info).
  - ``ArgSpec``: data type and optional default value the argument will be converted to.
  - ``Help``: help message that is shown for the option when ``usage/2`` is called.

The second parameter holds the list of arguments as passed to the ``main/1``
function in escripts. e.g.

     {port, $p, "port", {integer, 5432}, "Database server port"}

If the function is successful parsing the command line arguments it will return
a tuple containing the parsed options and the non-option arguments. The options
will be represented by a list of key-value pairs with the ``Name`` of the
option as *key* and the argument from the command line as *value*. If the option
doesn't have an argument, only the atom corresponding to its ``Name`` will be
added to the list of options. For the example given above we could get something
like ``{port, 5432}``. The non-option arguments are just a list of strings with
all the arguments that did not have corresponding options.

e.g. For a program named ``ex.escript`` with the following option specifications:

    OptSpec =
        [
         {host,   $h,        "host",    {string, "localhost"}, "Database server host"},
         {port,   $p,        "port",    integer,               "Database server port"},
         {dbname, undefined, "dbname",  {string, "users"},     "Database name"},
         {xml,    $x,        undefined, undefined,             "Output data in XML"},
         {file,   undefined, undefined, string,                "Output file"}
        ].

And this command line:

    Args = "-h myhost --port=1000 -x myfile.txt dummy1 dummy2"

Which could also be passed in the format the ``main/1`` function receives the arguments in escripts:

    Args = ["-h", "myhost", "--port=1000", "-x", "myfile.txt", "dummy1", "dummy2"].

The call to ``getopt:parse/2``:

    getopt:parse(OptSpec, Args).

Will return:

    {ok,{[{host,"myhost"},
          {port,1000},
          xml,
          {file,"myfile.txt"},
          {dbname,"users"}],
         ["dummy1","dummy2"]}}

Also, the call to ``getopt:usage/2``:

    getopt:usage(OptSpec, "ex1").

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
