-module(cli).

-export([parser/2, parser/5, parse_args/2, print_help/1,
         print_version/1, print_error/2]).

parser(Prog, Config) ->
    cli_parser:new(Prog, Config).

parser(Prog, Usage, Desc, OptionSpecs, Config) ->
    parser(
      Prog,
      [{usage, Usage},
       {desc, Desc},
       {options, parser_opts(OptionSpecs)}
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
