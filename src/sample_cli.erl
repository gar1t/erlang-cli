-module(sample_cli).

-export([main/1]).

main(Args) ->
    Parser = sample_parser(),
    case cli:parse_args(Args, Parser) of
        {ok, print_help}    -> cli:print_help(Parser);
        {ok, print_version} -> cli:print_version(Parser);
        {ok, Parsed}        -> handle_parsed_args(Parsed);
        {error, Err}        -> cli:print_error(Err)
    end.

sample_parser() ->
    cli:parser(
      "sample", "1.0",
      "A sample CLI using erlang-cli.",
      []).

handle_parsed_args(Args) ->
    io:format("TODO: handle args: ~p~n", [Args]).
