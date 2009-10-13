%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <jcomellas@novamens.com>
%%% @copyright (C) 2009, Novamens SA (http://www.novamens.com)
%%% @doc Parses command line options with a format similar to that of GNU getopt.
%%% @end
%%%-------------------------------------------------------------------
-module(getopt).

-include("getopt.hrl").
%% @headerfile "getopt.hrl"

-define(TAB_LENGTH, 8).
%% Indentation of the help messages in number of tabs.
-define(INDENTATION, 3).

%% @type option() = atom() | {atom(), getopt_arg()}. Option type and optional default argument.
-type option() :: atom() | {atom(), getopt_arg()}.
%% @type option_spec() = #option{}. Command line option specification.
-type option_spec() :: #option{}.

-export([parse/2, usage/2]).


-spec parse([option_spec()], string() | [string()]) -> {ok, {[option()], [string()]}} | {error, {Reason :: atom(), Data :: any()}}.
%%--------------------------------------------------------------------
%% @spec parse(OptSpecList::[option_spec()], Args::string() | [string()]) -> [option()].
%% @doc  Parse the command line options and arguments returning a list of tuples
%%       and/or atoms using the Erlang convention for sending options to a
%%       function.
%%--------------------------------------------------------------------
parse(OptSpecList, CmdLine) ->
    try
        Args = if
                   is_integer(hd(CmdLine)) ->
                       string:tokens(CmdLine, " \t\n");
                   true ->
                       CmdLine
               end,
        parse(OptSpecList, [], [], 0, Args)
    catch
        throw: {error, {_Reason, _Data}} = Error ->
            Error
    end.

-spec parse([option_spec()], [option()], [string()], integer(), [string()]) -> {ok, {[option()], [string()]}} | {error, {Reason :: atom(), Data:: any()}}.
%% Process long options.
parse(OptSpecList, OptAcc, ArgAcc, ArgPos, [[$-, $- | LongName] = OptStr | Tail]) ->
    {Option, Tail1} = get_option(OptSpecList, OptStr, LongName, #option.long, Tail),
    parse(OptSpecList, [Option | OptAcc], ArgAcc, ArgPos, Tail1);
%% Process short options.
parse(OptSpecList, OptAcc, ArgAcc, ArgPos, [[$-, ShortName] = OptStr | Tail]) ->
    {Option, Tail1} = get_option(OptSpecList, OptStr, ShortName, #option.short, Tail),
    parse(OptSpecList, [Option | OptAcc], ArgAcc, ArgPos, Tail1);
%% Process multiple short options with no argument.
parse(OptSpecList, OptAcc, ArgAcc, ArgPos, [[$- | ShortNameList] = OptStr | Tail]) ->
    NewOptAcc = lists:foldl(fun (ShortName, OptAcc1) ->
                                    [get_option_no_arg(OptSpecList, OptStr, ShortName, #option.short) | OptAcc1]
                            end, OptAcc, ShortNameList),
    parse(OptSpecList, NewOptAcc, ArgAcc, ArgPos, Tail);
%% Process non-option arguments.
parse(OptSpecList, OptAcc, ArgAcc, ArgPos, [Arg | Tail]) ->
    case find_non_option_arg(OptSpecList, ArgPos) of
        {value, #option{} = OptSpec} ->
            parse(OptSpecList, [convert_option_arg(OptSpec, Arg) | OptAcc], ArgAcc, ArgPos + 1, Tail);
        false ->
            parse(OptSpecList, OptAcc, [Arg | ArgAcc], ArgPos, Tail)
    end;
parse(OptSpecList, OptAcc, ArgAcc, _ArgPos, []) ->
    %% Once we have completed gathering the options we add the ones that were
    %% not present but had default arguments in the specification.
    {ok, {lists:reverse(append_default_args(OptSpecList, OptAcc)), lists:reverse(ArgAcc)}}.


-spec get_option([option_spec()], string(), string() | char(), integer(), [string()]) ->
    {option(), [string()]}.
%% @doc Retrieve the specification corresponding to an option matching a string
%%      received on the command line.
get_option(OptSpecList, OptStr, OptName, FieldPos, Tail) ->
    case lists:keysearch(OptName, FieldPos, OptSpecList) of
        {value, #option{name = Name, arg = ArgSpec} = OptSpec} ->
            case ArgSpec of
                undefined ->
                    {Name, Tail};
                _ ->
                    case Tail of
                        [Arg | Tail1] ->
                            {convert_option_arg(OptSpec, Arg), Tail1};
                                    [] ->
                            throw({error, {missing_option_arg, Name}})
                    end
            end;
        false ->
            throw({error, {invalid_option, OptStr}})
    end.

-spec get_option_no_arg([option_spec()], string(), string() | char(), integer()) -> option().
%% @doc Retrieve the specification corresponding to an option that has no
%%      argument and matches a string received on the command line.
get_option_no_arg(OptSpecList, OptStr, OptName, FieldPos) ->
    case lists:keysearch(OptName, FieldPos, OptSpecList) of
        {value, #option{name = Name, arg = undefined}} ->
            Name;
        {value, #option{name = Name}} ->
            throw({error, {missing_option_arg, Name}});
        false ->
            throw({error, {invalid_option, OptStr}})
    end.

-spec find_non_option_arg([option_spec()], integer()) -> {value, option_spec()} | false.
%% @doc Find the option for the discrete argument in position specified in the
%%      Pos argument.
find_non_option_arg([#option{short = undefined, long = undefined} = Opt | _Tail], 0) ->
     {value, Opt};
find_non_option_arg([#option{short = undefined, long = undefined} | Tail], Pos) ->
    find_non_option_arg(Tail, Pos - 1);
find_non_option_arg([_Head | Tail], Pos) ->
    find_non_option_arg(Tail, Pos);
find_non_option_arg([], _Pos) ->
    false.


-spec append_default_args([option_spec()], [option()]) -> [option()].
%% @doc Appends the default values of the options that are not present.
append_default_args([#option{name = Name, arg = {_Type, DefaultArg}} | Tail], OptAcc) ->
    append_default_args(Tail,
               case lists:keymember(Name, 1, OptAcc) of
                   false ->
                       [{Name, DefaultArg} | OptAcc];
                   _ ->
                       OptAcc
               end);
%% For options with no default argument.
append_default_args([_Head | Tail], OptAcc) ->
    append_default_args(Tail, OptAcc);
append_default_args([], OptAcc) ->
    OptAcc.


-spec convert_option_arg(option_spec(), string()) -> [option()].
%% @doc Convert the argument passed in the command line to the data type
%%      indicated byt the argument specification.
convert_option_arg(#option{name = Name, arg = ArgSpec}, Arg) ->
    try
        Converted = case ArgSpec of
                        {Type, _DefaultArg} ->
                            to_type(Type, Arg);
                        Type when is_atom(Type) ->
                            to_type(Type, Arg)
                    end,
        {Name, Converted}
    catch
        error:_ ->
            throw({error, {invalid_option_arg, {Name, Arg}}})
    end.

-spec to_type(atom(), string()) -> getopt_arg().
to_type(binary, Arg) ->
    list_to_binary(Arg);
to_type(atom, Arg) ->
    list_to_atom(Arg);
to_type(integer, Arg) ->
    list_to_integer(Arg);
to_type(float, Arg) ->
    list_to_float(Arg);
to_type(boolean, Arg) ->
    LowerArg = string:to_lower(Arg),
    (LowerArg =:= "true") orelse (LowerArg =:= "t") orelse
    (LowerArg =:= "yes") orelse (LowerArg =:= "y") orelse
    (LowerArg =:= "on") orelse (LowerArg =:= "enabled");
to_type(_Type, Arg) ->
    Arg.


-spec usage([option_spec()], string()) -> ok.
%%--------------------------------------------------------------------
%% @spec usage(OptSpecList :: option_spec_list(), ProgramName :: string()) -> ok.
%% @doc  Show a message on stdout indicating the command line options and
%%       arguments that are supported by the program.
%%--------------------------------------------------------------------
usage(OptSpecList, ProgramName) ->
    io:format("Usage: ~s~s~n~n~s~n", [ProgramName, usage_cmd_line(OptSpecList), usage_options(OptSpecList)]).


-spec usage_cmd_line([option_spec()]) -> string().
%% @doc Return a string with the syntax for the command line options and
%%      arguments.
usage_cmd_line(OptSpecList) ->
    usage_cmd_line(OptSpecList, []).

%% For options with short form and no argument.
usage_cmd_line([#option{short = Short, arg = undefined} | Tail], Acc) when Short =/= undefined ->
    usage_cmd_line(Tail, [[$\s, $[, $-, Short, $]] | Acc]);
%% For options with only long form and no argument.
usage_cmd_line([#option{long = Long, arg = undefined} | Tail], Acc) when Long =/= undefined ->
    usage_cmd_line(Tail, [[$\s, $[, $-, $-, Long, $]] | Acc]);
%% For options with short form and argument.
usage_cmd_line([#option{name = Name, short = Short} | Tail], Acc) when Short =/= undefined ->
    usage_cmd_line(Tail, [[$\s, $[, $-, Short, $\s, $<, atom_to_list(Name), $>, $]] | Acc]);
%% For options with only long form and argument.
usage_cmd_line([#option{name = Name, long = Long} | Tail], Acc) when Long =/= undefined ->
    usage_cmd_line(Tail, [[$\s, $[, $-, $-, Long, $\s, $<, atom_to_list(Name), $>, $]] | Acc]);
%% For options with neither short nor long form and argument.
usage_cmd_line([#option{name = Name, arg = ArgSpec} | Tail], Acc) when ArgSpec =/= undefined ->
    usage_cmd_line(Tail, [[$\s, $<, atom_to_list(Name), $>] | Acc]);
usage_cmd_line([], Acc) ->
    lists:flatten(lists:reverse(Acc)).


-spec usage_options([option_spec()]) -> string().
%% @doc Return a string with the help message for each of the options and
%%      arguments.
usage_options(OptSpecList) ->
    usage_options(OptSpecList, []).

%% Neither short nor long form (non-option argument).
usage_options([#option{name = Name, short = undefined, long = undefined} = Opt | Tail], Acc) ->
    usage_options(Tail, add_option_help(Opt, [$<, atom_to_list(Name), $>], Acc));
%% Only short form.
usage_options([#option{short = Short, long = undefined} = Opt | Tail], Acc) ->
    usage_options(Tail, add_option_help(Opt, [$-, Short], Acc));
%% Only long form.
usage_options([#option{short = undefined, long = Long} = Opt | Tail], Acc) ->
    usage_options(Tail, add_option_help(Opt, [$-, $-, Long], Acc));
%% Both short and long form.
usage_options([#option{short = Short, long = Long} = Opt | Tail], Acc) ->
    usage_options(Tail, add_option_help(Opt, [$-, Short, $,, $\s, $-, $-, Long], Acc));
usage_options([], Acc) ->
    lists:flatten(lists:reverse(Acc)).


-spec add_option_help(option_spec(), Prefix :: string(), Acc :: string()) -> string().
%% @doc Add the help message corresponding to an option specification to a list
%%      with the correct indentation.
add_option_help(#option{help = Help}, Prefix, Acc) when is_list(Help), Help =/= [] ->
    FlatPrefix = lists:flatten(Prefix),
    case ((?INDENTATION * ?TAB_LENGTH) - 2 - length(FlatPrefix)) of
        TabSize when TabSize > 0 ->
            Tab = lists:duplicate(ceiling(TabSize / ?TAB_LENGTH), $\t),
            [[$\s, $\s, FlatPrefix, Tab, Help, $\n] | Acc];
        _ ->
            %% The indentation for the option description is 3 tabs (i.e. 24 characters)
            %% IMPORTANT: Change the number of tabs below if you change the
            %%            value of the INDENTATION macro.
            [[$\t, $\t, $\t, Help, $\n], [$\s, $\s, FlatPrefix, $\n] | Acc]
    end;
add_option_help(_Opt, _Prefix, Acc) ->
    Acc.


-spec ceiling(float()) -> integer().
%% @doc Return the smallest integral valur not less than the argument.
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        %% Neg when Neg < 0 ->
        %%    T;
        Pos when Pos > 0 ->
            T + 1;
        _ ->
            T
    end.
