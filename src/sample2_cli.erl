-module(sample2_cli).

-export([main/1]).

-define(default_message, "Aye Chiuawa!").

main(Args) ->
    Parser = sample2_parser(),
    case cli:parse_args(Args, Parser) of
        {ok, print_help} -> cli:print_help(Parser);
        {ok, print_version} -> cli:print_version(Parser);
        {ok, Parsed} -> handle_parsed(Parsed, Parser);
        {error, Err} -> cli:print_error_and_halt(Err, Parser)
    end.

sample2_parser() ->
    cli:command_parser(
      "sample2",
      "[OPTION]... COMMAND [ARG]...",
      "Sample CLI that supports commands yo.\n",
      [{"list", "List directory contents", list_parser()},
       {"del",  "Delete directory contents", del_parser()}
      ],
      [{force, "-F, --force", "Used to confirm risky operations", [flag]}],
      [{version, "1.0"}]).

list_parser() ->
    cli:parser(
      "sample2-list",
      "[OPTION]... [DIR]",
      "List DIR or current dir if not specified. "
      "directory.",
      []).

del_parser() ->
    cli:parser(
      "sample2-del",
      "[OPTION]... [DIR]",
      "Delete DIR or current dir if not specified.",
      []).

handle_parsed({_Opts, []}, _Parser) ->
    print_message(?default_message);
handle_parsed({_Opts, [Msg]}, _Parser) ->
    print_message(Msg);
handle_parsed(_, Parser) ->
    cli:print_usage_error_and_halt(Parser).

print_message(Msg) ->
    io:format("~s~n", [Msg]).
