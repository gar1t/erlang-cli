-module(sample_cli).

-export([main/1]).

main(Args) ->
    Parser = sample_parser(),
    case cli:parse_args(Args, Parser) of
        ok                  -> cli:print_help(Parser);
        {ok, print_help}    -> cli:print_help(Parser);
        {ok, print_version} -> cli:print_version(Parser);
        {ok, Parsed}        -> handle_parsed_args(Parsed);
        {error, Err}        -> cli:print_error(Err)
    end.

sample_parser() ->
    cli:parser(
      "sample",
      ("1.0\n"
       "Copyright (C) 2016 Garrett Smith\n"
       "License GPLv3+: GNU GPL version 3 or later "
       "<http://gnu.org/licenses/gpl.html>. This is free software: "
       "you are free to change and redistribute it. There is NO WARRANTY, "
       "to the extent permitted by law.\n"
       "\n"
       "Written by Garrett Smith."),
      ("A sample CLI using erlang-cli.\n"
       "\n"
       "This program illustrates various capabilities of erlang-cli. It's "
       "sweet. You'll know this when you try it."),
      [cli:arg(msg, "message to print", [optional]),
       cli:arg(caps, "print message in caps", [{flag, "-C, --caps"}]),
       cli:arg(x, "the X factor", [{flag, "-X"}]),
       cli:arg(y, "the Y factor", [{option, "-Y"}, {default, "123"}]),
       cli:arg(
         some_long_option,
         "this is some really long option - not sure what to make "
         "of it really; super cool, or super weird?",
         [{option, "-L, --super-long-option"}]),
       cli:arg(
         maybe_value_option,
         "you can specify a value here or not - up to you!",
         [option, arg_optional, {metavar, "MYSTERY"}])
      ]).

handle_parsed_args(Args) ->
    io:format("TODO: handle args: ~p~n", [Args]).
