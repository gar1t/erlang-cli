-module(cli_help).

-export([print_help/1, print_help/2, print_help/3,
         print_version/1, print_version/2, print_version/3,
         print_error/2, print_error/3,
         print_usage_error/1, print_usage_error/2]).

-export([format_opt_name/1]).

-define(default_page_width, 79).
-define(default_opt_desc_col, 30).

-record(fmt, {page_width, opt_desc_col}).

%% ===================================================================
%% Print help
%% ===================================================================

print_help(Parser) ->
    print_help(standard_error, Parser, []).

print_help(Device, Parser) ->
    print_help(Device, Parser, []).

print_help(Device, Parser, Opts) ->
    Fmt = init_fmt(Opts),
    print_usage(Device, Parser),
    print_program_desc(Device, Parser, Fmt),
    maybe_print_commands(
      cli_parser:is_command_parser(Parser),
      Device, Parser, Fmt),
    print_options(Device, Parser, Fmt).

init_fmt(Opts) ->
    Opt = fun(Name, Default) -> proplists:get_value(Name, Opts, Default) end,
    #fmt{
       page_width=Opt(page_width, ?default_page_width),
       opt_desc_col=Opt(opt_desc_col, ?default_opt_desc_col)
      }.

%% -------------------------------------------------------------------
%% Usage
%% -------------------------------------------------------------------

print_usage(Device, Parser) ->
    UsageLines = usage_lines(Parser),
    Prog = cli_parser:prog(Parser),
    print_usage_lines(Device, UsageLines, Prog, first).

usage_lines(Parser) ->
    split_lines(cli_parser:usage(Parser)).

print_usage_lines(Device, [Line|Rest], Prog, LineType) ->
    Prefix = usage_line_prefix(LineType),
    io:format(Device, "~s ~s ~s~n", [Prefix, Prog, Line]),
    print_usage_lines(Device, Rest, Prog, more);
print_usage_lines(_Device, [], _Prog, _LineType) ->
    ok.

usage_line_prefix(first) -> "Usage:";
usage_line_prefix(more)  -> "   or:".

%% -------------------------------------------------------------------
%% Program desc
%% -------------------------------------------------------------------

print_program_desc(Device, Parser, Fmt) ->
    io:format(Device, "~s~n", [formatted_program_desc(Parser, Fmt)]).

formatted_program_desc(Parser, #fmt{page_width=Width}) ->
    Pars = split_lines(program_desc(Parser)),
    prettypr:format(pars_doc(Pars), Width, Width).

program_desc(Parser) ->
    case cli_parser:desc(Parser) of
        undefined -> "";
        Desc -> Desc
    end.

%% -------------------------------------------------------------------
%% Commands
%% -------------------------------------------------------------------

maybe_print_commands(true, Device, Parser, Fmt) ->
    io:format(Device, "Commands:~n", []),
    lists:foreach(
      fun(Cmd) -> print_visible_command(Cmd, Device, Fmt) end,
      cli_parser:commands(Parser)),
    io:format(Device, "~n", []);
maybe_print_commands(false, _Device, _Parser, _Fmt) ->
    ok.

print_visible_command({Name, Help, CmdParser}, Device, Fmt) ->
    case cli_parser:visible(CmdParser) of
        true -> print_command(Device, Name, Help, Fmt);
        false -> ok
    end.

print_command(Device, Name, Help, Fmt) ->
    print_opt_name_with_padding(Device, format_command_name(Name), Fmt),
    print_opt_desc(Device, Help, Fmt).

format_command_name(Name) ->
    io_lib:format("  ~s", [Name]).

%% -------------------------------------------------------------------
%% Options
%% -------------------------------------------------------------------

print_options(Device, Parser, Fmt) ->
    io:format(Device, "Options:~n", []),
    print_parser_opts(Device, cli_parser:options(Parser), Fmt),
    print_help_and_version_opts(Device, Parser).

print_parser_opts(Device, Opts, Fmt) ->
    lists:foreach(fun(Opt) -> print_visible_opt(Opt, Device, Fmt) end, Opts).

print_visible_opt(Opt, Device, Fmt) ->
    case cli_opt:visible(Opt) of
        true -> print_opt(Device, Opt, Fmt);
        false -> ok
    end.

print_opt(Device, Opt, Fmt) ->
    print_opt_name_with_padding(Device, format_opt_name(Opt), Fmt),
    print_opt_desc(Device, format_opt_desc(Opt, Fmt), Fmt).

format_opt_name(Opt) ->
    Short = cli_opt:short(Opt),
    Long = cli_opt:long(Opt),
    Meta = {cli_opt:has_arg(Opt), cli_opt:metavar(Opt)},
    io_lib:format(
      "~s~s~s",
      [opt_short(Short, Long, Meta),
       opt_short_long_delim(Short, Long),
       opt_long(Long, Meta)]).

opt_short(undefined, _, _) ->
    "    ";
opt_short(Short, undefined, {no, _}) ->
    io_lib:format("  ~s", [Short]);
opt_short(Short, undefined, {yes, Metavar}) ->
    io_lib:format("  ~s ~s", [Short, Metavar]);
opt_short(Short, undefined, {optional, Metavar}) ->
    io_lib:format("  ~s [~s]", [Short, Metavar]);
opt_short(Short, _, _) ->
    io_lib:format("  ~s", [Short]).

opt_short_long_delim(undefined, _Long) -> "  ";
opt_short_long_delim(_Short, undefined) -> "";
opt_short_long_delim(_Short, _Long) -> ", ".

opt_long(undefined, _) ->
    "";
opt_long(Long, {no, _}) ->
    Long;
opt_long(Long, {yes, Metavar}) ->
    io_lib:format("~s=~s", [Long, Metavar]);
opt_long(Long, {optional, Metavar}) ->
    io_lib:format("~s[=~s]", [Long, Metavar]).

print_opt_name_with_padding(Device, FormattedName, Fmt) ->
    io:format(Device, FormattedName, []),
    pad_to_opt_desc(Device, FormattedName, Fmt).

pad_to_opt_desc(Device, FormattedName, #fmt{opt_desc_col=StartCol}) ->
    case StartCol - iolist_size(FormattedName) of
        Line1Padding when Line1Padding >= 0 ->
            io:format(Device, string:copies(" ", Line1Padding), []);
        _ ->
            io:format(Device, "~n", []),
            io:format(Device, string:copies(" ", StartCol), [])
    end.

format_opt_desc(Opt, #fmt{page_width=Page, opt_desc_col=ColStart}) ->
    Desc = cli_opt:desc(Opt),
    Width = Page - ColStart,
    prettypr:format(prettypr:text_par(Desc), Width).

print_opt_desc(Device, Desc, Fmt) ->
    [Line1|Rest] = split_lines(Desc),
    io:format(Device, Line1, []),
    io:format(Device, "~n", []),
    print_indented_opt_lines(Device, Fmt, Rest).

print_indented_opt_lines(Device, #fmt{opt_desc_col=Padding}=Fmt, [Line|Rest]) ->
    io:format(Device, string:copies(" ", Padding), []),
    io:format(Device, Line, []),
    io:format(Device, "~n", []),
    print_indented_opt_lines(Device, Fmt, Rest);
print_indented_opt_lines(_Device, _Help, []) ->
    ok.

print_help_and_version_opts(Device, Parser) ->
    print_help_opt(Device),
    maybe_print_version_opt(has_version(Parser), Device).

print_help_opt(Device) ->
    io:format(
      Device, "      --help     print this help and exit~n", []).

has_version(Parser) -> cli_parser:version(Parser) /= undefined.

maybe_print_version_opt(true, Device) ->
    io:format(
      Device, "      --version  print version information and exit~n", []);
maybe_print_version_opt(false, _) ->
    ok.

%% ===================================================================
%% Print version
%% ===================================================================

print_version(Parser) ->
    print_version(standard_error, Parser, []).

print_version(Device, Parser) ->
    print_version(Device, Parser, []).

print_version(Device, Parser, Opts) ->
    Prog = cli_parser:prog(Parser),
    {Version, Extra} = split_parser_version(Parser),
    print_program_and_version(Device, Prog, Version),
    print_version_extra(Device, Extra, init_fmt(Opts)).

split_parser_version(Parser) ->
    [Version|Extra] = split_lines(cli_parser:version(Parser)),
    {Version, Extra}.

print_program_and_version(Device, Prog, Version) ->
    io:format(Device, "~s ~s~n", [Prog, Version]).

print_version_extra(_Device, [], _Fmt) -> ok;
print_version_extra(Device, Extra, Fmt) ->
    io:format(Device, formatted_version_extra(Extra, Fmt), []).

formatted_version_extra(Pars, #fmt{page_width=Width}) ->
    prettypr:format(pars_doc(Pars), Width, Width).

%% ===================================================================
%% Print error
%% ===================================================================

print_error(Err, Parser) ->
    print_error(standard_error, Err, Parser).

print_error(Device, Err, Parser) ->
    {SuggestHelp, Msg} = format_error_msg(Err),
    print_prog_msg(Device, Parser, Msg),
    maybe_print_help_suggestion(SuggestHelp, Device, Parser).

print_prog_msg(Device, Parser, Msg) ->
    Prog = cli_parser:prog(Parser),
    io:format(Device, "~s: ~s~n", [Prog, Msg]).

maybe_print_help_suggestion(true, Device, Parser) ->
    Prog = cli_parser:prog(Parser),
    io:format(Device, "Try '~s --help' for more information.~n", [Prog]);
maybe_print_help_suggestion(false, _Device, _Parser) ->
    ok.

format_error_msg({unknown_opt, Name}) ->
    {true, io_lib:format("unrecognized option '~s'", [Name])};
format_error_msg({missing_opt_arg, _Key, Name}) ->
    {true, io_lib:format("option '~s' requires an argument", [Name])};
format_error_msg({unexpected_opt_arg, _Key, Name}) ->
    {true, io_lib:format("option '~s' doesn't allow an argument", [Name])};
format_error_msg({unknown_command, Name}) ->
    {true, io_lib:format("unrecognized command '~s'", [Name])};
format_error_msg(missing_command) ->
    {true, "this program requires a command"};
format_error_msg({unexpected_pos_arg, Arg}) ->
    {true, io_lib:format("unexpected argument '~s'", [Arg])};
format_error_msg(missing_pos_arg) ->
    {true, "missing one or more arguments"};
format_error_msg(Msg) when is_list(Msg); is_binary(Msg) ->
    {false, Msg}.

%% ===================================================================
%% Print usage error
%% ===================================================================

print_usage_error(Parser) ->
    print_usage_error(standard_error, Parser).

print_usage_error(Device, Parser) ->
    Prog = cli_parser:prog(Parser),
    print_usage(Device, Parser),
    io:format(Device, "Try '~s --help' for more information.~n", [Prog]).

%% ===================================================================
%% Helpers
%% ===================================================================

split_lines(Str) ->
    re:split(Str, "\n", [{return, list}]).

pars_doc(Pars) ->
    prettypr:par([prettypr:break(text_par_or_empty(Par)) || Par <- Pars]).

text_par_or_empty("")           -> prettypr:empty();
text_par_or_empty("!!" ++ Text) -> prettypr:text(Text);
text_par_or_empty(Text)         -> prettypr:text_par(Text).
