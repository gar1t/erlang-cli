-module(cli).

-export([parser/4, parser/5, command_parser/5, command_parser/6,
         parse_args/2, print_help/1, print_version/1, print_error/2,
         print_usage_error/1, main/3, main_error/1, main_error/2]).

-define(default_exit_code, 2).

parser(Prog, Usage, Desc, OptionSpec) ->
    parser(Prog, Usage, Desc, OptionSpec, []).

parser(Prog, Usage, Desc, OptionSpec, Config) ->
    cli_parser:new(
      Prog,
      [{usage, Usage},
       {desc, Desc},
       {options, parser_opts(OptionSpec)}
       |Config]).

command_parser(Prog, Usage, Desc, OptionSpec, Commands) ->
    parser(Prog, Usage, Desc, OptionSpec, [{commands, Commands}]).

command_parser(Prog, Usage, Desc, OptionSpec, Commands, Config) ->
    parser(Prog, Usage, Desc, OptionSpec, [{commands, Commands}|Config]).

parser_opts(Specs) ->
    [parser_opt(Spec) || Spec <- Specs].

parser_opt({Key, Name}) ->
    cli_opt:new(Key, [{name, Name}]);
parser_opt({Key, Name, Desc}) ->
    cli_opt:new(Key, [{name, Name}, {desc, Desc}]);
parser_opt({Key, Name, Desc, Opts}) ->
    cli_opt:new(Key, [{name, Name}, {desc, Desc}|Opts]).

parse_args(Args, Parser) ->
    cli_parser:parse_args(Args, Parser).

print_help(Parser) ->
    cli_help:print_help(Parser).

print_version(Parser) ->
    cli_help:print_version(Parser).

print_error(Err, Parser) ->
    cli_help:print_error(Err, Parser).

print_usage_error(Parser) ->
    cli_help:print_usage_error(Parser).

main(Args, Parser, HandleParsed) ->
    cli_main:main(Args, Parser, HandleParsed).

main_error(Msg) ->
    cli_main:main_error(Msg).

main_error(ExitCode, Msg) ->
    cli_main:main_error(ExitCode, Msg).
