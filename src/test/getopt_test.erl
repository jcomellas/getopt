%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <jcomellas@novamens.com>
%%% @copyright (C) 2009, Novamens SA (http://www.novamens.com)
%%% @doc Parses command line options with a format similar to that of GNU getopt.
%%%-------------------------------------------------------------------

-module(getopt_test).
-author('Juan Jose Comellas <jcomellas@novamens.com>').

-include("getopt.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(getopt, [parse/2, usage/2]).


%%%-------------------------------------------------------------------
%%% UNIT TESTS
%%%-------------------------------------------------------------------

%%% Test for the getopt/1 function
parse_1_test_() ->
    Short =
        #option{name     = short,
                short    = $a,
                help     = "Option with only short form and no argument"
               },
    ShortArg =
        #option{name     = short_arg,
                short    = $b,
                arg      = string,
                help     = "Option with only short form and argument"
               },
    ShortDefArg =
        #option{name     = short_def_arg,
                short    = $c,
                arg      = {string, "default"},
                help     = "Option with only short form and default argument"
               },
    ShortInt =
        #option{name     = short_int,
                short    = $e,
                arg      = integer,
                help     = "Option with only short form and integer argument"
               },
    Long =
        #option{name     = long,
                long     = "long",
                help     = "Option with only long form and no argument"
               },
    LongArg =
        #option{name     = long_arg,
                long     = "long-arg",
                arg      = string,
                help     = "Option with only long form and argument"
               },
    LongDefArg =
        #option{name     = long_def_arg,
                long     = "long-def-arg",
                arg      = {string, "default"},
                help     = "Option with only long form and default argument"
               },
    LongInt =
        #option{name     = long_int,
                long     = "long-int",
                arg      = integer,
                help     = "Option with only long form and integer argument"
               },
    ShortLong =
        #option{name     = short_long,
                short    = $g,
                long     = "short-long",
                help     = "Option with short form, long form and no argument"
               },
    ShortLongArg =
        #option{name     = short_long_arg,
                short    = $h,
                long     = "short-long-arg",
                arg      = string,
                help     = "Option with short form, long form and argument"
               },
    ShortLongDefArg =
        #option{name     = short_long_def_arg,
                short    = $i,
                long     = "short-long-def-arg",
                arg      = {string, "default"},
                help     = "Option with short form, long form and default argument"
               },
    ShortLongInt =
        #option{name     = short_long_int,
                short    = $j,
                long     = "short-long-int",
                arg      = integer,
                help     = "Option with short form, long form and integer argument"
               },
    
    [
     {"No options and no arguments",
      ?_assertMatch({ok, {[], []}}, parse([], []))},
     {"Options and no arguments",
      ?_assertMatch({ok, {[], []}}, parse([Short], []))},
     {"No options and single argument",
      ?_assertMatch({ok, {[], ["arg1"]}}, parse([], ["arg1"]))},
     {"No options and arguments",
      ?_assertMatch({ok, {[], ["arg1", "arg2"]}}, parse([], ["arg1", "arg2"]))},
     {"Unused options and arguments",
      ?_assertMatch({ok, {[], ["arg1", "arg2"]}}, parse([Short], ["arg1", "arg2"]))},
     %% Options with only the short form
     {Short#option.help,           ?_assertMatch({ok, {[short], []}}, parse([Short], [[$-, Short#option.short]]))},
     {ShortArg#option.help,        ?_assertMatch({ok, {[{short_arg, "value"}], []}}, parse([ShortArg], [[$-, ShortArg#option.short], "value"]))},
     {ShortDefArg#option.help,     ?_assertMatch({ok, {[{short_def_arg, "default"}], []}}, parse([ShortDefArg], []))},
     {ShortInt#option.help,        ?_assertMatch({ok, {[{short_int, 100}], []}}, parse([ShortInt], [[$-, ShortInt#option.short], "100"]))},
     {"Short form option and arguments",
      ?_assertMatch({ok, {[short], ["arg1", "arg2"]}}, parse([Short], [[$-, Short#option.short], "arg1", "arg2"]))},
     {"Short form option and arguments (unsorted)",
      ?_assertMatch({ok, {[short], ["arg1", "arg2"]}}, parse([Short], ["arg1", [$-, Short#option.short], "arg2"]))},
     %% Options with only the long form
     {Long#option.help,            ?_assertMatch({ok, {[long], []}}, parse([Long], ["--long"]))},
     {LongArg#option.help,         ?_assertMatch({ok, {[{long_arg, "value"}], []}}, parse([LongArg], ["--long-arg", "value"]))},
     {LongDefArg#option.help,      ?_assertMatch({ok, {[{long_def_arg, "default"}], []}}, parse([LongDefArg], []))},
     {LongInt#option.help,         ?_assertMatch({ok, {[{long_int, 100}], []}}, parse([LongInt], ["--long-int", "100"]))},
     {"Long form option and arguments",
      ?_assertMatch({ok, {[long], ["arg1", "arg2"]}}, parse([Long], ["--long", "arg1", "arg2"]))},
     {"Long form option and arguments (unsorted)",
      ?_assertMatch({ok, {[long], ["arg1", "arg2"]}}, parse([Long], ["arg1", "--long", "arg2"]))},
     %% Options with both the short and long form
     {ShortLong#option.help,       ?_assertMatch({ok, {[short_long], []}}, parse([ShortLong], [[$-, ShortLong#option.short]]))},
     {ShortLong#option.help,       ?_assertMatch({ok, {[short_long], []}}, parse([ShortLong], ["--short-long"]))},
     {ShortLongArg#option.help,    ?_assertMatch({ok, {[{short_long_arg, "value"}], []}}, parse([ShortLongArg], [[$-, ShortLongArg#option.short], "value"]))},
     {ShortLongArg#option.help,    ?_assertMatch({ok, {[{short_long_arg, "value"}], []}}, parse([ShortLongArg], ["--short-long-arg", "value"]))},
     {ShortLongDefArg#option.help, ?_assertMatch({ok, {[{short_long_def_arg, "default"}], []}}, parse([ShortLongDefArg], []))},
     {ShortLongInt#option.help,    ?_assertMatch({ok, {[{short_long_int, 1234}], []}}, parse([ShortLongInt], [[$-, ShortLongInt#option.short], "1234"]))},
     {ShortLongInt#option.help,    ?_assertMatch({ok, {[{short_long_int, 1234}], []}}, parse([ShortLongInt], ["--short-long-int", "1234"]))},
     {"Option with only short form and invalid integer argument",
      ?_assertEqual({error, {invalid_option_arg, {ShortInt#option.name, "value"}}}, parse([ShortInt], [[$-, ShortInt#option.short], "value"]))}
    ].
