-module(cli_arg).

-export([new/2, new/3]).

-export([name/1, desc/1, arg_type/1, value_type/1, default/1,
         short_opt/1, long_opt/1, metavar/1, value_required/1]).

-record(arg, {key, desc, arg_type, value_type, default,
              short_opt, long_opt, metavar}).

-define(required, '$required').

%% ===================================================================
%% New
%% ===================================================================

new(Key, Desc) ->
    new(Key, Desc, []).

new(Key, Desc, Opts) ->
    ArgType = arg_type_from_opts(Opts),
    {ShortOpt, LongOpt} = short_long_from_opts(ArgType, Opts, Key),
    ValueType = value_type_from_opts(Opts),
    Default = default_from_opts(Opts, ArgType),
    Metavar = metavar_from_opts(Opts, ValueType),
    #arg{
       key=Key,
       desc=Desc,
       arg_type=ArgType,
       value_type=ValueType,
       default=Default,
       short_opt=ShortOpt,
       long_opt=LongOpt,
       metavar=Metavar
      }.

short_long_from_opts(option, Opts, Key) ->
    find_short_long(Opts, Key);
short_long_from_opts(positional, _Opts, _Key) ->
    {undefined, undefined}.

find_short_long([{name, Name}|_], _Key) ->
    short_long_from_name(Name);
find_short_long([{option, Name}|_], _Key) ->
    short_long_from_name(Name);
find_short_long([{flag, Name}|_], _Key) ->
    short_long_from_name(Name);
find_short_long([_|Rest], Key) ->
    find_short_long(Rest, Key);
find_short_long([], Key) ->
    {undefined, long_opt_from_key(Key)}.

short_long_from_name(Name) ->
    Pattern = "^(?:(-.))?(?:, )?(?:(--.+))?$",
    case re:run(Name, Pattern, [{capture, all_but_first, list}]) of
        {match, ["", Long]} -> {undefined, Long};
        {match, [Short, Long]} -> {Short, Long};
        {match, [Short]} -> {Short, undefined};
        nomatch -> error({bad_option_name, Name})
    end.

long_opt_from_key(Key) ->
    "--" ++ replace(atom_to_list(Key), "_", "-").

replace(Str, Replace, With) ->
    re:replace(Str, Replace, With, [{return, list}]).

arg_type_from_opts(Opts) ->
    case is_flag_or_option_from_opts(Opts) of
        true -> option;
        false -> positional
    end.

is_flag_or_option_from_opts(Opts) ->
    is_option_from_opts(flag, Opts) orelse is_option_from_opts(option, Opts).

is_option_from_opts(OptionType, Opts) ->
    case opt_val(OptionType, Opts, false) of
        Flag when is_boolean(Flag) -> Flag;
        Name when is_list(Name) -> true
    end.

value_type_from_opts(Opts) ->
    Default = fun() -> default_value_type(Opts) end,
    opt_val(value_type, Opts, Default).

default_value_type(Opts) ->
    case is_option_from_opts(flag, Opts) of
        true -> bool;
        false -> str
    end. 

default_from_opts(Opts, ArgType) ->
    Default = fun() -> default_default(Opts, ArgType) end,
    opt_val(default, Opts, Default).

default_default(Opts, ArgType) ->
    case arg_type_from_opts(Opts) of
        positional -> ?required;
        option -> default_for_option_type(ArgType)
    end.

default_for_option_type(bool) -> false;
default_for_option_type(_)    -> ?required.

metavar_from_opts(Opts, ArgType) ->
    Default = fun() -> default_metavar(ArgType) end,
    opt_val(metavar, Opts, Default).

default_metavar(bool) -> undefined;
default_metavar(_)    -> "VALUE".

%% ===================================================================
%% Attrs
%% ===================================================================

name(#arg{key=Key}) ->
    string:to_upper(atom_to_list(Key)).

desc(#arg{desc=Desc}) -> Desc.

arg_type(#arg{arg_type=ArgType}) -> ArgType.

value_type(#arg{value_type=ValueType}) -> ValueType.

default(#arg{default=Default}) -> Default.

short_opt(#arg{short_opt=Short}) -> Short.

long_opt(#arg{long_opt=Long}) -> Long.

metavar(#arg{metavar=Metavar}) -> Metavar.

value_required(#arg{default=?required}) -> true;
value_required(_) -> false.

%% ===================================================================
%% Helpers
%% ===================================================================

opt_val(Key, Opts, Default) ->
    case proplists:get_value(Key, Opts, '$undefined') of
        '$undefined' when is_function(Default) -> Default();
        '$undefined' -> Default;
        Val -> Val
    end.
