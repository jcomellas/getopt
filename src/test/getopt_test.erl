%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2009 Juan Jose Comellas
%%% @doc Parses command line options with a format similar to that of GNU getopt.
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------

-module(getopt_test).
-author('juanjo@comellas.org').

-include_lib("eunit/include/eunit.hrl").

-import(getopt, [parse/2, usage/2]).

-define(NAME(Opt), element(1, Opt)).
-define(SHORT(Opt), element(2, Opt)).
-define(LONG(Opt), element(3, Opt)).
-define(ARG_SPEC(Opt), element(4, Opt)).
-define(HELP(Opt), element(5, Opt)).


%%%-------------------------------------------------------------------
%%% UNIT TESTS
%%%-------------------------------------------------------------------

%%% Test for the getopt/1 function
parse_1_test_() ->
    Short           = {short,              $a,        undefined,            undefined,                      "Option with only short form and no argument"},
    Short2          = {short2,             $b,        undefined,            undefined,                      "Second option with only short form and no argument"},
    Short3          = {short3,             $c,        undefined,            undefined,                      "Third option with only short form and no argument"},
    ShortArg        = {short_arg,          $d,        undefined,            string,                         "Option with only short form and argument"},
    ShortDefArg     = {short_def_arg,      $e,        undefined,            {string, "default-short"},      "Option with only short form and default argument"},
    ShortInt        = {short_int,          $f,        undefined,            integer,                        "Option with only short form and integer argument"},
    Long            = {long,               undefined, "long",               undefined,                      "Option with only long form and no argument"},
    LongArg         = {long_arg,           undefined, "long-arg",           string,                         "Option with only long form and argument"},
    LongDefArg      = {long_def_arg,       undefined, "long-def-arg",       {string, "default-long"},       "Option with only long form and default argument"},
    LongInt         = {long_int,           undefined, "long-int",           integer,                        "Option with only long form and integer argument"},
    ShortLong       = {short_long,         $g,        "short-long",         undefined,                      "Option with short form, long form and no argument"},
    ShortLongArg    = {short_long_arg,     $h,        "short-long-arg",     string,                         "Option with short form, long form and argument"},
    ShortLongDefArg = {short_long_def_arg, $i,        "short-long-def-arg", {string, "default-short-long"}, "Option with short form, long form and default argument"},
    ShortLongInt    = {short_long_int,     $j,        "short-long-int",     integer,                        "Option with short form, long form and integer argument"},
    NonOptArg       = {non_opt_arg,        undefined, undefined,            undefined,                      "Non-option argument"},
    NonOptInt       = {non_opt_int,        undefined, undefined,            integer,                        "Non-option integer argument"},
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
         [$-, ?SHORT(Short)],
         [$-, ?SHORT(ShortArg)], "value1",
         [$-, ?SHORT(ShortInt)], "100",
         [$-, ?SHORT(Short2), ?SHORT(Short3)],
         "--long",
         "--long-arg", "value2",
         "--long-int", "101",
         [$-, ?SHORT(ShortLong)],
         "--short-long-arg", "value3",
         "--short-long-int", "103",
         "value4",
         "104",
         "dummy1",
         "dummy2"
        ],
    CombinedOpts =
        [
         ?NAME(Short),
         {?NAME(ShortArg), "value1"},
         {?NAME(ShortInt), 100},
         ?NAME(Short2),
         ?NAME(Short3),
         ?NAME(Long),
         {?NAME(LongArg), "value2"},
         {?NAME(LongInt), 101},
         ?NAME(ShortLong),
         {?NAME(ShortLongArg), "value3"},
         {?NAME(ShortLongInt), 103},
         {?NAME(NonOptArg), "value4"},
         {?NAME(NonOptInt), 104},
         {?NAME(ShortDefArg), "default-short"},
         {?NAME(LongDefArg), "default-long"},
         {?NAME(ShortLongDefArg), "default-short-long"}
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
     {?HELP(Short),           ?_assertEqual({ok, {[short], []}}, parse([Short], [[$-, ?SHORT(Short)]]))},
     {?HELP(ShortArg),        ?_assertEqual({ok, {[{short_arg, "value"}], []}}, parse([ShortArg], [[$-, ?SHORT(ShortArg)], "value"]))},
     {?HELP(ShortDefArg),     ?_assertMatch({ok, {[{short_def_arg, "default-short"}], []}}, parse([ShortDefArg], []))},
     {?HELP(ShortInt),        ?_assertEqual({ok, {[{short_int, 100}], []}}, parse([ShortInt], [[$-, ?SHORT(ShortInt)], "100"]))},
     {"Unsorted multiple short form options and arguments in a single string",
      ?_assertMatch({ok, {[short, short2, short3], ["arg1", "arg2"]}}, parse([Short, Short2, Short3], "arg1 -abc arg2"))},
     {"Short form option and arguments",
      ?_assertMatch({ok, {[short], ["arg1", "arg2"]}}, parse([Short], [[$-, ?SHORT(Short)], "arg1", "arg2"]))},
     {"Short form option and arguments (unsorted)",
      ?_assertMatch({ok, {[short], ["arg1", "arg2"]}}, parse([Short], ["arg1", [$-, ?SHORT(Short)], "arg2"]))},
     %% Options with only the long form
     {?HELP(Long),            ?_assertMatch({ok, {[long], []}}, parse([Long], ["--long"]))},
     {?HELP(LongArg),         ?_assertMatch({ok, {[{long_arg, "value"}], []}}, parse([LongArg], ["--long-arg", "value"]))},
     {?HELP(LongDefArg),      ?_assertMatch({ok, {[{long_def_arg, "default-long"}], []}}, parse([LongDefArg], []))},
     {?HELP(LongInt),         ?_assertMatch({ok, {[{long_int, 100}], []}}, parse([LongInt], ["--long-int", "100"]))},
     {"Long form option and arguments",
      ?_assertMatch({ok, {[long], ["arg1", "arg2"]}}, parse([Long], ["--long", "arg1", "arg2"]))},
     {"Long form option and arguments (unsorted)",
      ?_assertMatch({ok, {[long], ["arg1", "arg2"]}}, parse([Long], ["arg1", "--long", "arg2"]))},
     %% Options with both the short and long form
     {?HELP(ShortLong),       ?_assertEqual({ok, {[short_long], []}}, parse([ShortLong], [[$-, ?SHORT(ShortLong)]]))},
     {?HELP(ShortLong),       ?_assertMatch({ok, {[short_long], []}}, parse([ShortLong], ["--short-long"]))},
     {?HELP(ShortLongArg),    ?_assertEqual({ok, {[{short_long_arg, "value"}], []}}, parse([ShortLongArg], [[$-, ?SHORT(ShortLongArg)], "value"]))},
     {?HELP(ShortLongArg),    ?_assertMatch({ok, {[{short_long_arg, "value"}], []}}, parse([ShortLongArg], ["--short-long-arg", "value"]))},
     {?HELP(ShortLongDefArg), ?_assertMatch({ok, {[{short_long_def_arg, "default-short-long"}], []}}, parse([ShortLongDefArg], []))},
     {?HELP(ShortLongInt),    ?_assertEqual({ok, {[{short_long_int, 1234}], []}}, parse([ShortLongInt], [[$-, ?SHORT(ShortLongInt)], "1234"]))},
     {?HELP(ShortLongInt),    ?_assertMatch({ok, {[{short_long_int, 1234}], []}}, parse([ShortLongInt], ["--short-long-int", "1234"]))},
     %% Non-option arguments
     {?HELP(NonOptArg),       ?_assertMatch({ok, {[{non_opt_arg, "value"}], []}}, parse([NonOptArg], ["value"]))},
     {?HELP(NonOptInt),       ?_assertMatch({ok, {[{non_opt_int, 1234}], []}}, parse([NonOptInt], ["1234"]))},
     {"Declared and undeclared non-option arguments",
     ?_assertMatch({ok, {[{non_opt_arg, "arg1"}], ["arg2", "arg3"]}}, parse([NonOptArg], ["arg1", "arg2", "arg3"]))},
     %% Combined
     {"Combined short, long and non-option arguments",
      ?_assertEqual({ok, {CombinedOpts, CombinedRest}}, parse(CombinedOptSpecs, CombinedArgs))},
     {"Option with only short form and invalid integer argument",
      ?_assertEqual({error, {invalid_option_arg, {?NAME(ShortInt), "value"}}}, parse([ShortInt], [[$-, ?SHORT(ShortInt)], "value"]))}
    ].
