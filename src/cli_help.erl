-module(cli_help).

-export([print_help/1, print_help/2,
         print_version/1, print_version/2,
         print_error/2, print_error/3,
         print_usage_error/1, print_usage_error/2]).

-define(page_width, 79).
-define(opt_desc_col, 30).

%% ===================================================================
%% Print help
%% ===================================================================

print_help(Parser) ->
    print_help(standard_error, Parser).

print_help(Device, Parser) ->
    print_usage(Device, Parser),
    print_program_desc(Device, Parser),
    maybe_print_commands(
      cli_parser:is_command_parser(Parser),
      Device, Parser),
    print_options(Device, Parser).

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

print_program_desc(undefined, _Parser) ->
    ok;
print_program_desc(Device, Parser) ->
    io:format(Device, "~s~n", [formatted_program_desc(Parser)]).

formatted_program_desc(Parser) ->
    Pars = split_lines(cli_parser:desc(Parser)),
    prettypr:format(pars_doc(Pars), ?page_width, ?page_width).

%% -------------------------------------------------------------------
%% Commands
%% -------------------------------------------------------------------

maybe_print_commands(true, Device, Parser) ->
    io:format(Device, "Commands:~n", []),
    print_commands(Device, cli_parser:commands(Parser)),
    io:format(Device, "~n", []);
maybe_print_commands(false, _Device, _Parser) ->
    ok.

print_commands(Device, [{Name, Help, _Parser}|Rest]) ->
    print_opt_name_with_padding(Device, format_command_name(Name)),
    print_opt_desc(Device, Help),
    print_commands(Device, Rest);
print_commands(_Device, []) ->
    ok.

format_command_name(Name) ->
    io_lib:format("  ~s", [Name]).

%% -------------------------------------------------------------------
%% Options
%% -------------------------------------------------------------------

print_options(Device, Parser) ->
    io:format(Device, "Options:~n", []),
    print_parser_opts(Device, cli_parser:options(Parser)),
    print_help_and_version_opts(Device, Parser).

print_parser_opts(Device, [Opt|Rest]) ->
    print_opt(Device, Opt),
    print_parser_opts(Device, Rest);
print_parser_opts(_Device, []) ->
    ok.

print_opt(Device, Opt) ->
    print_opt_name_with_padding(Device, format_opt_name(Opt)),
    print_opt_desc(Device, format_opt_desc(Opt)).

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

print_opt_name_with_padding(Device, FormattedName) ->
    io:format(Device, FormattedName, []),
    pad_to_opt_desc(Device, FormattedName).

pad_to_opt_desc(Device, FormattedName) ->
    case ?opt_desc_col - iolist_size(FormattedName) of
        Line1Padding when Line1Padding >= 0 ->
            io:format(Device, string:copies(" ", Line1Padding), []);
        _ ->
            io:format(Device, "~n", []),
            io:format(Device, string:copies(" ", ?opt_desc_col), [])
    end.

format_opt_desc(Opt) ->
    Desc = cli_opt:desc(Opt),
    Width = ?page_width - ?opt_desc_col,
    prettypr:format(prettypr:text_par(Desc), Width).

print_opt_desc(Device, Desc) ->
    [Line1|Rest] = split_lines(Desc),
    io:format(Device, Line1, []),
    io:format(Device, "~n", []),
    print_indented_opt_lines(Device, Rest).

print_indented_opt_lines(Device, [Line|Rest]) ->
    io:format(Device, string:copies(" ", ?opt_desc_col), []),
    io:format(Device, Line, []),
    io:format(Device, "~n", []),
    print_indented_opt_lines(Device, Rest);
print_indented_opt_lines(_Device, []) ->
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
    print_version(standard_error, Parser).

print_version(Device, Parser) ->
    Prog = cli_parser:prog(Parser),
    {Version, Extra} = split_parser_version(Parser),
    print_program_and_version(Device, Prog, Version),
    print_version_extra(Device, Extra).

split_parser_version(Parser) ->
    [Version|Extra] = split_lines(cli_parser:version(Parser)),
    {Version, Extra}.

print_program_and_version(Device, Prog, Version) ->
    io:format(Device, "~s ~s~n", [Prog, Version]).

print_version_extra(_Device, []) -> ok;
print_version_extra(Device, Extra) ->
    io:format(Device, formatted_version_extra(Extra), []).

formatted_version_extra(Pars) ->
    prettypr:format(pars_doc(Pars), ?page_width, ?page_width).

%% ===================================================================
%% Print error
%% ===================================================================

print_error(Err, Parser) ->
    print_error(standard_error, Err, Parser).

print_error(Device, Err, Parser) ->
    Prog = cli_parser:prog(Parser),
    ErrMsg = format_error_msg(Err),
    io:format(Device, "~s: ~s~n", [Prog, ErrMsg]),
    io:format(Device, "Try '~s --help' for more information.~n", [Prog]).

format_error_msg({unknown_opt, Name}) ->
    io_lib:format("unrecognized option '~s'", [Name]);
format_error_msg({missing_arg, _Key, Name}) ->
    io_lib:format("option '~s' requires an argument", [Name]);
format_error_msg({unexpected_arg, _Key, Name}) ->
    io_lib:format("option '~s' doesn't allow an argument", [Name]);
format_error_msg({unknown_command, Name}) ->
    io_lib:format("unrecognized command '~s'", [Name]);
format_error_msg(missing_command) ->
    "this program requires a command".

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

text_par_or_empty("") -> prettypr:empty();
text_par_or_empty(Text) -> prettypr:text_par(Text).
