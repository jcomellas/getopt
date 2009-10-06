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
-define(HELP_INDENTATION, 4 * ?TAB_LENGTH).

%% @type option()      = atom() | {atom(), getopt_arg()}. Option type and optional default argument.
-type option()        :: atom() | {atom(), getopt_arg()}.
%% @type option_spec() = [#option{}]. Command line options specification.
-type option_spec()   :: [#option{}].
%% @type option_list() = [option()]. List of option types.
-type option_list()   :: [option()].
%% @type arg_list()    = [getopt_arg()]. List of arguments returned to the calling function.
-type arg_list()      :: [getopt_arg()].

-export([parse/2, usage/2]).


-spec parse(option_spec(), [string()]) -> option_list().
%%--------------------------------------------------------------------
%% @spec parse(OptSpec::option_spec(), Args::[string()]) -> option_list().
%% @doc  Parse the command line options and arguments returning a list of tuples
%%       and/or atoms using the Erlang convention for sending options to a
%%       function.
%%--------------------------------------------------------------------
parse(OptSpec, Args) ->
    catch parse(OptSpec, [], [], Args).

-spec parse(option_spec(), option_list(), arg_list(), [string()]) -> option_list().
%% Process short and long options
parse(OptSpec, OptAcc, ArgAcc, [("-" ++ OptTail) = Opt | Tail]) ->
    {SearchField, OptName} = case OptTail of
                                 "-" ++ Str ->
                                     {#option.long, Str};
                                 [Char] ->
                                     {#option.short, Char};
                                 _ ->
                                     throw({error, {invalid_option, Opt}})
                             end,

    case lists:keysearch(OptName, SearchField, OptSpec) of
        {value, #option{name = Name, arg = ArgSpec}} ->
            if
                ArgSpec =/= undefined ->
                    case Tail of
                        [Arg | Tail1] ->
                            parse(OptSpec, [convert_option_arg(Name, ArgSpec, Arg) | OptAcc], ArgAcc, Tail1);
                        [] ->
                            throw({error, {missing_option_arg, Name}})
                    end;
                true ->
                    parse(OptSpec, [Name | OptAcc], ArgAcc, Tail)
            end;
        false ->
            {error, {invalid_option, Opt}}
    end;
%% Process the discrete arguments
parse(OptSpec, OptAcc, ArgAcc, [Arg | Tail]) ->
    parse(OptSpec, OptAcc, [Arg | ArgAcc], Tail);
parse(OptSpec, OptAcc, ArgAcc, []) ->
    %% Once we have completed gathering the options that have short and long
    %% option strings we merge the remaining discrete arguments that were
    %% specified.
    {MergedOpts, MergedArgs} = merge_discrete_args(OptSpec, OptAcc, ArgAcc),
    %% Finally, we set the options that were not present to their default
    %% arguments.
    {ok, {lists:reverse(append_default_args(OptSpec, MergedOpts)), MergedArgs}}.



-spec merge_discrete_args(option_spec(), option_list(), arg_list()) -> {option_list(), arg_list()}.
%% @doc Merge the discrete arguments that were declared in the option
%%      specification to the list of options.
merge_discrete_args(OptSpec, Options, Args) ->
    merge_discrete_args(OptSpec, Args, 0, Options, []).

merge_discrete_args(OptSpec, [Head | Tail] = Args, Count, OptAcc, ArgAcc) ->
    case find_discrete_arg(OptSpec, 0) of
        {value, #option{name = Name, arg = ArgSpec}} ->
            merge_discrete_args(OptSpec, Tail, Count + 1, [convert_option_arg(Name, ArgSpec, Head) | OptAcc], ArgAcc);
        false ->
            %% If we could not find a discrete option specification that means
            %% that all the remaining elements in the list are discrete
            %% arguments.
            %% merge_discrete_args(OptSpec, Tail, Count, OptAcc, [Head | ArgAcc])
            {OptAcc, lists:foldl(fun (Arg, Acc) -> [Arg | Acc] end, ArgAcc, Args)}
    end;
merge_discrete_args(_OptSpec, [], _Count, OptAcc, ArgAcc) ->
    {OptAcc, ArgAcc}.


-spec find_discrete_arg(option_spec(), integer()) -> {value, #option{}} | false.
%% @doc Find the option for the discrete argument in position specified in the
%%      Pos argument.
find_discrete_arg([#option{short = undefined, long = undefined} = Opt | _Tail], 0) ->
     {value, Opt};
find_discrete_arg([#option{short = undefined, long = undefined} | Tail], Pos) ->
    find_discrete_arg(Tail, Pos - 1);
find_discrete_arg([_Head | Tail], Pos) ->
    find_discrete_arg(Tail, Pos);
find_discrete_arg([], _Pos) ->
    false.


-spec append_default_args(option_spec(), option_list()) -> option_list().
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


-spec convert_option_arg(atom(), getopt_arg_spec(), Arg :: string()) -> option_list().
%% @doc Convert the argument passed in the command line to the data type
%%      indicated byt the argument specification.
convert_option_arg(Name, ArgSpec, Arg) ->
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


-spec usage(option_spec(), string()) -> ok.
%%--------------------------------------------------------------------
%% @spec usage(OptSpec :: option_spec(), ProgramName :: string()) -> ok.
%% @doc  Show a message on stdout indicating the command line options and
%%       arguments that are supported by the program.
%%--------------------------------------------------------------------
usage(OptSpec, ProgramName) ->
    io:format("Usage: ~s~s~n~n~s~n", [ProgramName, usage_cmd_line(OptSpec), usage_options(OptSpec)]).


-spec usage_cmd_line(option_spec()) -> string().
%% @doc Return a string with the syntax for the command line options and
%%      arguments.
usage_cmd_line(OptSpec) ->
    usage_cmd_line(OptSpec, []).

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


-spec usage_options(option_spec()) -> string().
%% @doc Return a string with the help message for each of the options and
%%      arguments.
usage_options(OptSpec) ->
    usage_options(OptSpec, []).

%% Neither short nor long form (discrete argument).
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


-spec add_option_help(#option{}, Prefix :: list(), Acc :: list()) -> list().
%% @doc Add the help message corresponding to an option specification to a list
%%      with the correct indentation.
add_option_help(#option{help = Help}, Prefix, Acc) when is_list(Help), Help =/= [] ->
    FlatPrefix = lists:flatten(Prefix),
    case (?HELP_INDENTATION - 2 - length(FlatPrefix)) of
        TabSize when TabSize > 0 ->
            Tab = lists:duplicate(ceiling(TabSize / ?TAB_LENGTH), $\t),
            [[$\s, $\s, FlatPrefix, Tab, Help, $\n] | Acc];
        _ ->
            %% The indentation for the option description is 4 tabs (i.e. 32 characters)
            [[$\t, $\t, $\t, $\t, Help, $\n], [$\s, $\s, FlatPrefix, $\n] | Acc]
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
