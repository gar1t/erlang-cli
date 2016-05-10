-module(cli_opt).

-export([new/2]).

-export([key/1, desc/1, has_arg/1, short/1, long/1, metavar/1]).

-export([int_val/4]).

-record(opt, {key, desc, has_arg, short, long, metavar}).

%% ===================================================================
%% New
%% ===================================================================

new(Key, Opts) ->
    Desc = desc_from_opts(Opts),
    HasArg = has_arg_from_opts(Opts),
    {Short, Long} = short_long_from_opts(Opts, Key),
    Metavar = metavar_from_opts(Opts),
    #opt{
       key=Key,
       desc=Desc,
       has_arg=HasArg,
       short=Short,
       long=Long,
       metavar=Metavar
      }.

desc_from_opts(Opts) ->
    proplists:get_value(desc, Opts, "").

short_long_from_opts(Opts, Key) ->
    short_long_from_name(name_from_opts(Opts, Key)).

name_from_opts(Opts, Key) ->
    Default = fun() -> long_opt_from_key(Key) end,
    opt_val(name, Opts, Default).

long_opt_from_key(Key) ->
    "--" ++ replace(atom_to_list(Key), "_", "-").

short_long_from_name(Name) ->
    Pattern = "^(?:(-.))?(?:, )?(?:(--.+))?$",
    case re:run(Name, Pattern, [{capture, all_but_first, list}]) of
        {match, ["", Long]}    -> {undefined, Long};
        {match, [Short, Long]} -> {Short, Long};
        {match, [Short]}       -> {Short, undefined};
        nomatch -> error({bad_option_name, Name})
    end.

has_arg_from_opts(Opts) ->
    Default = fun() -> default_has_arg(Opts) end,
    opt_val(has_arg, Opts, Default).

default_has_arg(Opts) ->
    apply_boolopt_map(
      [{flag,         no},
       {no_arg,       no},
       {optional_arg, optional},
       {'_',          yes}],
      Opts).

metavar_from_opts(Opts) ->
    Default = fun() -> default_metavar(Opts) end,
    opt_val(metavar, Opts, Default).

default_metavar(Opts) ->
    apply_boolopt_map(
      [{flag,   undefined},
       {no_arg, undefined},
       {'_',    "VALUE"}],
      Opts).

%% ===================================================================
%% Attrs
%% ===================================================================

key(#opt{key=Key}) -> Key.

desc(#opt{desc=Desc}) -> Desc.

has_arg(#opt{has_arg=HasArg}) -> HasArg.

short(#opt{short=Short}) -> Short.

long(#opt{long=Long}) -> Long.

metavar(#opt{metavar=Metavar}) -> Metavar.

%% ===================================================================
%% Converters
%% ===================================================================

int_val(Key, Opts, Default, ErrMsg) ->
    case proplists:get_value(Key, Opts) of
        undefined -> Default;
        Str -> str_to_int(Str, ErrMsg)
    end.

str_to_int(Str, ErrMsg) ->
    try
        list_to_integer(Str)
    catch
        error:badarg -> throw({error, ErrMsg})
    end.

%% ===================================================================
%% Helpers
%% ===================================================================

opt_val(Key, Opts, Default) ->
    case proplists:get_value(Key, Opts, '$undefined') of
        '$undefined' when is_function(Default) -> Default();
        '$undefined' -> Default;
        Val -> Val
    end.

apply_boolopt_map([{'_', Result}|_], _Opts) -> Result;
apply_boolopt_map([{Opt, Result}|Rest], Opts) ->
    case proplists:get_bool(Opt, Opts) of
        true  -> Result;
        false -> apply_boolopt_map(Rest, Opts)
    end.

replace(Str, Replace, With) ->
    re:replace(Str, Replace, With, [{return, list}]).
