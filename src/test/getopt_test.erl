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
    Short2 =
        #option{name     = short2,
                short    = $b,
                help     = "Second option with only short form and no argument"
               },
    Short3 =
        #option{name     = short3,
                short    = $c,
                help     = "Third ption with only short form and no argument"
               },
    ShortArg =
        #option{name     = short_arg,
                short    = $d,
                arg      = string,
                help     = "Option with only short form and argument"
               },
    ShortDefArg =
        #option{name     = short_def_arg,
                short    = $e,
                arg      = {string, "default-short"},
                help     = "Option with only short form and default argument"
               },
    ShortInt =
        #option{name     = short_int,
                short    = $f,
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
                arg      = {string, "default-long"},
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
                arg      = {string, "default-short-long"},
                help     = "Option with short form, long form and default argument"
               },
    ShortLongInt =
        #option{name     = short_long_int,
                short    = $j,
                long     = "short-long-int",
                arg      = integer,
                help     = "Option with short form, long form and integer argument"
               },
    NonOptArg =
        #option{name     = non_opt_arg,
                help     = "Non-option argument"
               },
    NonOptInt =
        #option{name     = non_opt_int,
                arg      = integer,
                help     = "Non-option integer argument"
               },
    CombinedOptSpecs =
        [
         Short,
         ShortArg,
         ShortInt,
         Short2,
         Short3,
         Long,
         LongArg,
         LongInt,
         ShortLong,
         ShortLongArg,
         ShortLongInt,
         NonOptArg,
         NonOptInt,
         ShortDefArg,
         LongDefArg,
         ShortLongDefArg
        ],
    CombinedArgs =
        [
         [$-, Short#option.short],
         [$-, ShortArg#option.short], "value1",
         [$-, ShortInt#option.short], "100",
         [$-, Short2#option.short, Short3#option.short],
         "--long",
         "--long-arg", "value2",
         "--long-int", "101",
         [$-, ShortLong#option.short],
         "--short-long-arg", "value3",
         "--short-long-int", "103",
         "value4",
         "104",
         "dummy1",
         "dummy2"
        ],
    CombinedOpts =
        [
         Short#option.name,
         {ShortArg#option.name, "value1"},
         {ShortInt#option.name, 100},
         Short2#option.name,
         Short3#option.name,
         Long#option.name,
         {LongArg#option.name, "value2"},
         {LongInt#option.name, 101},
         ShortLong#option.name,
         {ShortLongArg#option.name, "value3"},
         {ShortLongInt#option.name, 103},
         {NonOptArg#option.name, "value4"},
         {NonOptInt#option.name, 104},
         {ShortDefArg#option.name, "default-short"},
         {LongDefArg#option.name, "default-long"},
         {ShortLongDefArg#option.name, "default-short-long"}
        ],
    CombinedRest = ["dummy1", "dummy2"],
         
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
     {ShortDefArg#option.help,     ?_assertMatch({ok, {[{short_def_arg, "default-short"}], []}}, parse([ShortDefArg], []))},
     {ShortInt#option.help,        ?_assertMatch({ok, {[{short_int, 100}], []}}, parse([ShortInt], [[$-, ShortInt#option.short], "100"]))},
     {"Unsorted multiple short form options and arguments in a single string",
      ?_assertMatch({ok, {[short, short2, short3], ["arg1", "arg2"]}}, parse([Short, Short2, Short3], "arg1 -abc arg2"))},
     {"Short form option and arguments",
      ?_assertMatch({ok, {[short], ["arg1", "arg2"]}}, parse([Short], [[$-, Short#option.short], "arg1", "arg2"]))},
     {"Short form option and arguments (unsorted)",
      ?_assertMatch({ok, {[short], ["arg1", "arg2"]}}, parse([Short], ["arg1", [$-, Short#option.short], "arg2"]))},
     %% Options with only the long form
     {Long#option.help,            ?_assertMatch({ok, {[long], []}}, parse([Long], ["--long"]))},
     {LongArg#option.help,         ?_assertMatch({ok, {[{long_arg, "value"}], []}}, parse([LongArg], ["--long-arg", "value"]))},
     {LongDefArg#option.help,      ?_assertMatch({ok, {[{long_def_arg, "default-long"}], []}}, parse([LongDefArg], []))},
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
     {ShortLongDefArg#option.help, ?_assertMatch({ok, {[{short_long_def_arg, "default-short-long"}], []}}, parse([ShortLongDefArg], []))},
     {ShortLongInt#option.help,    ?_assertMatch({ok, {[{short_long_int, 1234}], []}}, parse([ShortLongInt], [[$-, ShortLongInt#option.short], "1234"]))},
     {ShortLongInt#option.help,    ?_assertMatch({ok, {[{short_long_int, 1234}], []}}, parse([ShortLongInt], ["--short-long-int", "1234"]))},
     %% Non-option arguments
     {NonOptArg#option.help,       ?_assertMatch({ok, {[{non_opt_arg, "value"}], []}}, parse([NonOptArg], ["value"]))},
     {NonOptInt#option.help,       ?_assertMatch({ok, {[{non_opt_int, 1234}], []}}, parse([NonOptInt], ["1234"]))},
     {"Declared and undeclared non-option arguments",
     ?_assertMatch({ok, {[{non_opt_arg, "arg1"}], ["arg2", "arg3"]}}, parse([NonOptArg], ["arg1", "arg2", "arg3"]))},
     %% Combined
     {"Combined short, long and non-option arguments",
      ?_assertEqual({ok, {CombinedOpts, CombinedRest}}, parse(CombinedOptSpecs, CombinedArgs))},
     {"Option with only short form and invalid integer argument",
      ?_assertEqual({error, {invalid_option_arg, {ShortInt#option.name, "value"}}}, parse([ShortInt], [[$-, ShortInt#option.short], "value"]))}
    ].
