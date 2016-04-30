-module(sample_cli).

-export([main/1]).

main(Args) ->
    %cli_debug:trace_function(cli_parser, opt),
    Parser = sample_parser(),
    case cli:parse_args(Args, Parser) of
        ok                  -> cli:print_help(Parser);
        {ok, print_help}    -> cli:print_help(Parser);
        {ok, print_version} -> cli:print_version(Parser);
        {ok, Parsed}        -> handle_parsed_args(Parsed);
        {error, Err}        -> cli:print_error_and_halt(Err, Parser)
    end.

sample_parser() ->
    cli:parser(
      "sample",
      "[OPTION]... [MSG]\n[OPTION]... WHOOPPEEE",
      "A sample CLI using erlang-cli.\n"
      "\n"
      "This program illustrates various capabilities of erlang-cli. It's "
      "sweet. You'll know this when you try it.",
      [{caps, "-C, --caps", "print message in caps", [flag]},
       {x,    "-X",         "the X factor",          [flag]},
       {y,    "-Y",         "the Y factor",          [{metavar, "FACTOR"}]},
       {z,    "-Z, --zed",  "the Z factor",          [no_arg]},
       {some_long_option, "-L, --super-long-option",
        "this is some really long option - not sure what to make "
        "of it really; super cool, or super weird?"},
       {maybe_value_option, "-M, --maybe-value",
        "you can specify a value here or not - up to you!",
        [optional_arg, {metavar, "MYSTERY"}]
       }
      ],
      [{version,
        "1.0\n"
        "Copyright (C) 2016 Garrett Smith\n"
        "License GPLv3+: GNU GPL version 3 or later "
        "<http://gnu.org/licenses/gpl.html>. This is free software: "
        "you are free to change and redistribute it. There is NO WARRANTY, "
        "to the extent permitted by law.\n"
        "\n"
        "Written by Garrett Smith."
       }
      ]).

handle_parsed_args({Opts, Args}) ->
    io:format("Options: ~p~n", [Opts]),
    io:format("Args:    ~p~n", [Args]).
