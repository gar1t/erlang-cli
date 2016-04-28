-module(cli).

-export([parser/4,
         arg/2, arg/3,
         option/2, option/3,
         flag/2, flag/3,
         parse_args/2, print_help/1, print_version/1]).

parser(Program, Version, Desc, Args) ->
    cli_parser:new(Program, Version, Desc, Args).

arg(Key, Desc) ->
    cli_arg:new(Key, Desc).

arg(Key, Desc, Opts) ->
    cli_arg:new(Key, Desc, Opts).

option(Key, Desc) ->
    option(Key, Desc, []).

option(Key, Desc, Opts) ->
    cli_arg:new(Key, Desc, [option|Opts]).

flag(Key, Desc) ->
    flag(Key, Desc, []).

flag(Key, Desc, Opts) ->
    cli_arg:new(Key, Desc, [flag|Opts]).

parse_args(Args, Parser) ->
    cli_parser3:parse_args(Args, Parser).

print_help(Parser) ->
    cli_help:print_help(Parser).

print_version(Parser) ->
    cli_help:print_version(Parser).
