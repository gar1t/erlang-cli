-module(sample2_cli).

-export([main/1]).

-define(default_message, "Ay Chiwawa!").

main(Args) ->
    Parser = sample2_parser(),
    case cli:parse_args(Args, Parser) of
        {ok, {print_help, P}}    -> cli:print_help(P);
        {ok, {print_version, P}} -> cli:print_version(P);
        {ok, Parsed}             -> handle_parsed(Parsed);
        {error, Err, P}          -> cli:print_error_and_halt(Err, P)
    end.

sample2_parser() ->
    cli:command_parser(
      "sample2",
      "[OPTION]... COMMAND [ARG]...",
      "Sample CLI that supports commands yo.\n",
      [{"list", "list directory contents", list_parser()},
       {"del",  "delete directory contents", del_parser()}
      ],
      [{force, "-F, --force", "confirm risky operations", [flag]}],
      [{version, "1.0"}]).

list_parser() ->
    cli:parser(
      "sample2 list",
      "[OPTION]... [DIR]",
      "List DIR or current dir if not specified. "
      "directory.",
      []).

del_parser() ->
    cli:parser(
      "sample2 del",
      "[OPTION]... [DIR]",
      "Delete DIR or current dir if not specified.",
      []).

handle_parsed({"list", Opts, Args}) ->
    handle_list(Opts, Args);
handle_parsed({"del", Opts, Args}) ->
    handle_del(Opts, Args).

handle_list(Opts, Args) ->
    io:format("TODO: list ~p ~p~n", [Opts, Args]).

handle_del(Opts, Args) ->
    io:format("TODO: del ~p ~p~n", [Opts, Args]).
