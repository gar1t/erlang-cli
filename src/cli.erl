-module(cli).

-export([parser/4, parse_args/2, print_help/1, print_version/1]).

-include("cli.hrl").

parser(Program, Version, Desc, Args) ->
    #parser{
       prog=Program,
       version=Version,
       desc=Desc,
       args=Args}.

parse_args(Args, Parser) ->
    cli_parse:parse_args(Args, Parser).

print_help(Parser) ->
    cli_help:print_help(Parser).

print_version(Parser) ->
    cli_help:print_version(Parser).
