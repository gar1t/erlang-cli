-module(cli).

-export([parser/4, parser/5, parse_args/2, print_help/1,
         print_version/1, print_error/2, print_error_and_halt/2,
         print_error_and_halt/3]).

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

print_error_and_halt(Err, Parser) ->
    print_error_and_halt(Err, Parser, ?default_exit_code).

print_error_and_halt(Err, Parser, ExitCode) ->
    print_error(Err, Parser),
    erlang:halt(ExitCode).

print_usage_error(Parser) ->
    cli_help:print_usage_error(Parser).

print_usage_error_and_halt(Parser) ->
    print_usage_error_and_halt(Parser, ?default_exit_code).

print_usage_error_and_halt(Parser, ExitCode) ->
    print_usage_error(Parser),
    erlang:halt(ExitCode).
