-module(cli_help).

-export([print_help/1, print_help/2, print_version/1, print_version/2]).

-define(page_width, 79).
-define(arg_desc_col, 30).

print_help(Parser) ->
    print_help(standard_error, Parser).

print_help(Device, Parser) ->
    print_usage_line(Device, Parser),
    print_program_desc(Device, Parser),
    print_options(Device, Parser).

print_usage_line(Device, Parser) ->
    print_prog_and_options(Device, Parser),
    print_arg_synopsis(Device, cli_parser:args(Parser)),
    print_lf(Device).

print_prog_and_options(Device, Parser) ->
    io:format(Device, "Usage: ~s [OPTION]...", [cli_parser:prog(Parser)]).

print_arg_synopsis(Device, [Arg|Rest]) ->
    maybe_print_positional(cli_arg:arg_type(Arg), Device, Arg),
    print_arg_synopsis(Device, Rest);
print_arg_synopsis(_Device, []) ->
    ok.

maybe_print_positional(positional, Device, Arg) ->
    print_sp(Device),
    print_positional(Device, cli_arg:name(Arg), cli_arg:value_required(Arg));
maybe_print_positional(option, _Device, _Arg) ->
    ok.

print_positional(Device, Name, true=_Required) ->
    io:format(Device, "~s", [Name]);
print_positional(Device, Name, false=_Required) ->
    io:format(Device, "[~s]", [Name]).

print_program_desc(Device, Parser) ->
    io:format(Device, "~s~n", [formatted_program_desc(Parser)]).

formatted_program_desc(Parser) ->
    Pars = split_lines(cli_parser:desc(Parser)),
    prettypr:format(pars_doc(Pars), ?page_width, ?page_width).

pars_doc(Pars) ->
    prettypr:par([prettypr:break(text_par_or_empty(Par)) || Par <- Pars]).

text_par_or_empty("") -> prettypr:empty();
text_par_or_empty(Text) -> prettypr:text_par(Text).

print_options(Device, Parser) ->
    io:format(Device, "Options:~n", []),
    print_parser_args(Device, cli_parser:options(Parser)),
    print_help_and_version_options(Device, Parser).

print_parser_args(Device, [Arg|Rest]) ->
    print_arg(Device, Arg),
    print_parser_args(Device, Rest);
print_parser_args(_Device, []) ->
    ok.

print_arg(Device, Arg) ->
    print_arg_name_with_padding(Device, format_arg_name(Arg)),
    print_arg_desc(Device, format_arg_desc(Arg)).

format_arg_name(Arg) ->
    Short = cli_arg:short_opt(Arg),
    Long = cli_arg:long_opt(Arg),
    Metavar = cli_arg:metavar(Arg),
    Required = cli_arg:value_required(Arg),
    io_lib:format(
      "~s~s~s",
      [arg_short(Short, Long, Metavar, Required),
       arg_short_long_delim(Short, Long),
       arg_long(Long, Metavar, Required)]).

arg_short(undefined, _, _, _) ->
    "    ";
arg_short(Short, undefined, undefined, _) ->
    io_lib:format("  ~s", [Short]);
arg_short(Short, undefined, Metavar, true) ->
    io_lib:format("  ~s ~s", [Short, Metavar]);
arg_short(Short, undefined, Metavar, false) ->
    io_lib:format("  ~s [~s]", [Short, Metavar]);
arg_short(Short, _, _, _) ->
    io_lib:format("  ~s", [Short]).

arg_short_long_delim(undefined, _Long) -> "  ";
arg_short_long_delim(_Short, undefined) -> "";
arg_short_long_delim(_Short, _Long) -> ", ".

arg_long(undefined, _, _) ->
    "";
arg_long(Long, undefined, _) ->
    Long;
arg_long(Long, Metavar, true) ->
    io_lib:format("~s=~s", [Long, Metavar]);
arg_long(Long, Metavar, false) ->
    io_lib:format("~s[=~s]", [Long, Metavar]).

print_arg_name_with_padding(Device, FormattedName) ->
    io:format(Device, FormattedName, []),
    pad_to_arg_desc(Device, FormattedName).

pad_to_arg_desc(Device, FormattedName) ->
    case ?arg_desc_col - iolist_size(FormattedName) of
        Line1Padding when Line1Padding >= 0 ->
            io:format(Device, string:copies(" ", Line1Padding), []);
        _ ->
            io:format(Device, "~n", []),
            io:format(Device, string:copies(" ", ?arg_desc_col), [])
    end.

format_arg_desc(Arg) ->
    Desc = cli_arg:desc(Arg),
    Width = ?page_width - ?arg_desc_col,
    prettypr:format(prettypr:text_par(Desc), Width).

print_arg_desc(Device, Desc) ->
    [Line1|Rest] = split_lines(Desc),
    io:format(Device, Line1, []),
    io:format(Device, "~n", []),
    print_indented_arg_lines(Device, Rest).

split_lines(Str) ->
    re:split(Str, "\n", [{return, list}]).

print_indented_arg_lines(Device, [Line|Rest]) ->
    io:format(Device, string:copies(" ", ?arg_desc_col), []),
    io:format(Device, Line, []),
    io:format(Device, "~n", []),
    print_indented_arg_lines(Device, Rest);
print_indented_arg_lines(_Device, []) ->
    ok.

print_help_and_version_options(Device, _Parser) ->
    io:format(
      Device, "      --help     print this help and exit~n", []),
    io:format(
      Device, "      --version  print version information and exit~n", []).

print_sp(Device) ->
    io:format(Device, " ", []).

print_lf(Device) ->
    io:format(Device, "~n", []).

print_version(Parser) ->
    print_version(standard_error, Parser).

print_version(Device, Parser) ->
    Prog = cli_parser:prog(Parser),
    Version = cli_parser:version(Parser),
    io:format(Device, "~s ~s~n", [Prog, Version]).
