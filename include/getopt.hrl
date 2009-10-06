%% @type getopt_arg_type() = 'atom' | 'binary' | 'bool' | 'float' | 'integer' | 'string'.
%% Atom indicating the data type that an argument can be converted to.
-type getopt_arg_type() :: 'atom' | 'binary' | 'boolean' | 'float' | 'integer' | 'string'.
%% @type getopt_arg()      = atom() | binary() | bool() | float() | integer() | string().
%% Data type that an argument can be converted to.
-type getopt_arg()      :: atom() | binary() | boolean() | float() | integer() | string().
%% @type getopt_arg_spec() = getopt_arg_type() | {getopt_arg_type(), getopt_arg()} | help | undefined.
%% Argument specification.
-type getopt_arg_spec() :: getopt_arg_type() | {getopt_arg_type(), getopt_arg()} | help | undefined.

%% @doc Record that defines the option specifications.
-record(option, {
          %% @doc Name of the option
          name              :: atom(),
          %% @doc Character for the short option (e.g. $i for -i)
          short             :: char() | undefined,
          %% @doc String for the long option (e.g. "info" for --info)
          long              :: string() | undefined,
          %% @doc Data type the argument will be converted to with an optional default value
          arg               :: getopt_arg_spec(),
          %% @doc Help message that is shown for the option when usage/2 is called.
          help              :: string() | undefined
         }).
