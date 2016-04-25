-module(cli_help).

-export([print_help/1, print_help/2, print_version/1, print_version/2]).

-include("cli.hrl").

print_help(Parser) ->
    print_help(standard_error, Parser).

print_help(Device, Parser) ->
    print_usage_line(Device, Parser),
    print_program_desc(Device, Parser),
    print_lf(Device),
    print_options(Device, Parser),
    print_commands(Device, Parser).

print_usage_line(Device, #parser{prog=Prog}) ->
    io:format(Device, "Usage: ~s [OPTION]...~n", [Prog]).

print_program_desc(Device, Parser) ->
    io:format(Device, "~s~n", [formatted_program_desc(Parser)]).

formatted_program_desc(#parser{desc=Desc}) when is_list(Desc) ->
    prettypr:format(prettypr:text_par(Desc), ?page_width).

print_options(Device, Parser) ->
    io:format(Device, "Options:~n", []),
    print_help_and_version_options(Device, Parser).

print_help_and_version_options(Device, _Parser) ->
    io:format(
      Device, "      --help     print this help and exit~n", []),
    io:format(
      Device, "      --version  print version information and exit~n", []).

print_commands(_Device, _Parser) ->
    todo.

print_lf(Device) ->
    io:format(Device, "~n", []).

print_version(Parser) ->
    print_version(standard_error, Parser).

print_version(Device, #parser{prog=Prog, version=Version}) ->
    io:format(Device, "~s ~s~n", [Prog, Version]).
