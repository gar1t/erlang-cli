-module(sample2_cli).

-export([main/1]).

-define(default_message, "Ay Chiwawa!").

main(Args) ->
    Code = cli:main(Args, parser(), fun handle_cmd/1),
    erlang:halt(Code).

parser() ->
    cli:command_parser(
      "sample2",
      "[OPTION]... COMMAND [ARG]...",
      "Sample CLI that supports commands yo.\n",
      [{force, "-F, --force", "confirm risky operations", [flag]}],
      [{"list", "list directory contents", list_parser()},
       {"del",  "delete directory contents", del_parser()}
      ],
      [{version, "1.0"}]).

list_parser() ->
    cli:parser(
      "sample2 list",
      "[OPTION]... [DIR]...",
      "List DIRs or current dir if not specified.",
      []).

del_parser() ->
    cli:parser(
      "sample2 del",
      "[OPTION]... [DIR]...",
      "Delete DIRs or current dir if not specified. Requires '--force'.",
      []).

handle_cmd({"list", Opts, Args}) ->
    handle_list(Opts, Args);
handle_cmd({"del", Opts, Args}) ->
    handle_del(Opts, Args).

handle_list(Opts, Args) ->
    io:format("TODO: list ~p ~p~n", [Opts, Args]).

handle_del(Opts, Args) ->
    io:format("TODO: del ~p ~p~n", [Opts, Args]).
