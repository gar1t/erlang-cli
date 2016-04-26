-module(cli_tests).

-export([run/0]).

run() ->
    test_parser_attrs(),
    test_arg_facade().

test_parser_attrs() ->
    io:format("parser_attrs: "),

    %% Create parser

    Foo = cli:arg(foo, "foo arg"),
    Bar = cli:arg(bar, "bar flag", [flag]),
    P = cli:parser("myprog", "1", "Sample program", [Foo, Bar]),

    %% Parser attrs

    "myprog" = cli_parser:prog(P),
    "1" = cli_parser:version(P),
    "Sample program" = cli_parser:desc(P),
    [Foo, Bar] = cli_parser:args(P),
    [Bar] = cli_parser:options(P),

    io:format("OK~n").

test_arg_facade() ->
    io:format("arg_facade: "),

    %% Positional w/defaults

    P1 = cli:arg(pos_1, "positional arg 1"),
    "POS_1" = cli_arg:name(P1),
    "positional arg 1" = cli_arg:desc(P1),
    positional = cli_arg:arg_type(P1),
    str = cli_arg:value_type(P1),
    true = cli_arg:value_required(P1),

    %% Option w/defaults

    O1 = cli:option(opt_1, "option 1"),
    "OPT_1" = cli_arg:name(O1),
    "option 1" = cli_arg:desc(O1),
    option = cli_arg:arg_type(O1),
    str = cli_arg:value_type(O1),
    true = cli_arg:value_required(O1),

    %% Flag w/defaults

    F1 = cli:flag(flag_1, "flag 1"),
    "FLAG_1" = cli_arg:name(F1),
    "flag 1" = cli_arg:desc(F1),
    bool = cli_arg:value_type(F1),
    false = cli_arg:value_required(F1),
    
    io:format("OK~n").
