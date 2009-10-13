%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <jcomellas@novamens.com>
%%% @copyright (C) 2009, Novamens SA (http://www.novamens.com)
%%% @doc Example file for the getopt module.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(ex1).

-include("include/getopt.hrl").

-export([test/0, test/1]).

test() ->
    test("-U myuser -P mypassword --host myhost -x -o myfile.dump mydb dummy1").


test(CmdLine) ->
    OptSpecList = option_spec(),

    io:format("For command line: ~p~n"
              "getopt:parse/2 returns:~n~n", [CmdLine]),
    case getopt:parse(OptSpecList, CmdLine) of
        {ok, {Options, NonOptArgs}} ->
            io:format("Options:~n  ~p~nNon-option arguments:~n  ~p~n", [Options, NonOptArgs]);
        {error, {Reason, Data}} ->
            io:format("Error: ~s ~p~n~n", [Reason, Data]),
            getopt:usage(OptSpecList, "ex1")
    end.


option_spec() ->
    CurrentUser = os:getenv("USER"),
    [
     #option{name     = help,
             short    = $?,
             long     = "help",
             help     = "Show the program options"
            },
     #option{name     = username,
             short    = $U,
             long     = "username",
             arg      = string,
             help     = "Username to connect to the database"
            },
     #option{name     = password,
             short    = $P,
             long     = "password",
             arg      = {string, CurrentUser},
             help     = "Password to connect to the database"
            },
     #option{name     = host,
             short    = $h,
             long     = "host",
             arg      = {string, "localhost"},
             help     = "Database server host name or IP address"
            },
     #option{name     = port,
             short    = $p,
             long     = "port",
             arg      = {integer, 1000},
             help     = "Database server port"
            },
     #option{name     = output_file,
             short    = $o,
             long     = "output-file",
             arg      = string,
             help     = "File where the data will be saved to"
            },
     #option{name     = xml,
             short    = $x,
             long     = "xml",
             help     = "Output data as XML"
            },
     #option{name     = dbname,
             arg      = string,
             help     = "Database name"
            }
    ].
