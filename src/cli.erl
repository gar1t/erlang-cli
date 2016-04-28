-module(cli).

-export([parser/1, parser/4, parse_args/2, print_help/1, print_version/1]).

parser(Config) ->
    cli_parser:new(Config).

parser(Usage, Desc, OptionSpecs, ParserConfig) ->
    parser(
      [{usage, Usage},
       {desc, Desc},
       {options, parser_opts(OptionSpecs)}
       |ParserConfig]).

parser_opts(Specs) ->
    [parser_opt(Spec) || Spec <- Specs].

parser_opt({Key, Name}) ->
    cli_opt:new(Key, [{name, Name}]);
parser_opt({Key, Name, Desc}) ->
    cli_opt:new(Key, [{name, Name}, {desc, Desc}]);
parser_opt({Key, Name, Desc, Opts}) ->
    cli_opt:new(Key, [{name, Name}, {desc, Desc}|Opts]).

parse_args(Args, Parser) ->
    cli_parser3:parse_args(Args, Parser).

print_help(Parser) ->
    cli_help:print_help(Parser).

print_version(Parser) ->
    cli_help:print_version(Parser).
