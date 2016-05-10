-module(cli_tests).

-compile([nowarn_unused_function]).

-export([run/0]).

run() ->
    test_cli_opt(),
    test_parse_args(),
    test_parse_pos_args(),
    test_opt_convert().

test_cli_opt() ->
    io:format("cli_opt: "),

    Opt = fun(Key, Opts) -> opt_props(cli_opt:new(Key, Opts)) end,

    #{key      := foo,
      desc     := "",
      has_arg  := yes,
      short    := undefined,
      long     := "--foo",
      metavar  := "VALUE"} = Opt(foo, []),

    #{desc := "foo option"} = Opt(foo, [{desc, "foo option"}]),

    #{has_arg := yes}      = Opt(bar, []),
    #{has_arg := no}       = Opt(bar, [flag]),
    #{has_arg := no}       = Opt(bar, [no_arg]),
    #{has_arg := optional} = Opt(bar, [optional_arg]),

    #{short := undefined, long := "--baz"}   = Opt(baz, []),
    #{short := "-Z",      long := "--baz"}   = Opt(baz, [{name, "-Z, --baz"}]),
    #{short := "-Z",      long := undefined} = Opt(baz, [{name, "-Z"}]),
    #{short := undefined, long := "--baz"}   = Opt(baz, [{name, "--baz"}]),

    #{metavar := "VALUE"}   = Opt(foo, []),
    #{metavar := undefined} = Opt(foo, [flag]),
    #{metavar := undefined} = Opt(foo, [no_arg]),
    #{metavar := "FOO"}     = Opt(foo, [{metavar, "FOO"}]),

    io:format("OK~n").

opt_props(O) ->
    #{key     => cli_opt:key(O),
      desc    => cli_opt:desc(O),
      has_arg => cli_opt:has_arg(O),
      short   => cli_opt:short(O),
      long    => cli_opt:long(O),
      metavar => cli_opt:metavar(O)}.

test_parse_args() ->
    io:format("parse_args: "),

    P = fun(Args, OptSpec) -> element(1, parse_args(Args, OptSpec)) end,

    %% Null parser (NP)
    NP = fun(Args) -> P(Args, []) end,
    {ok, {[], []}} = NP([]),

    %% Unspecified options
    {ok, {[], []}} = P([], [{foo, "-F"}, {bar, "-B"}]),

    %% Positional args with single foo option (Foo)
    Foo = fun(Args) -> P(Args, [{foo, "-F"}]) end,
    {ok, {[],             ["foo", "bar"]}} = Foo(["foo", "bar"]),
    {ok, {[{foo, "foo"}], ["bar"]}}        = Foo(["-F", "foo", "bar"]),
    {ok, {[],             ["-F", "foo"]}}  = Foo(["--", "-F", "foo"]),

    %% Option with required arg (Req)
    Req = fun(Args) -> P(Args, [{foo, "-F, --foo"}]) end,
    {error, {missing_opt_arg, foo, "--foo"}} = Req(["--foo"]),
    {ok, {[{foo, "123"}], []}}               = Req(["--foo", "123"]),
    {ok, {[{foo, "123"}], []}}               = Req(["--foo=123"]),
    {error, {missing_opt_arg, foo, "-F"}}    = Req(["-F"]),
    {ok, {[{foo, "123"}], []}}               = Req(["-F", "123"]),
    {ok, {[{foo, "123"}], []}}               = Req(["-F123"]),

    %% Option with no arg (NoArg)
    NoArg = fun(Args) -> P(Args, [{foo, "-F, --foo", "", [no_arg]}]) end,
    {ok, {[foo], []}}                           = NoArg(["--foo"]),
    {ok, {[foo], ["123"]}}                      = NoArg(["--foo", "123"]),
    {error, {unexpected_opt_arg, foo, "--foo"}} = NoArg(["--foo=123"]),
    {ok, {[foo], []}}                           = NoArg(["-F"]),
    {ok, {[foo], ["123"]}}                      = NoArg(["-F", "123"]),
    {error, {unknown_opt, "-1"}}                = NoArg(["-F123"]),

    %% Option with optional arg (Opt)
    Opt = fun(Args) -> P(Args, [{foo, "-F, --foo", "", [optional_arg]}]) end,
    {ok, {[{foo, ""}],    []}} = Opt(["--foo"]),
    {ok, {[{foo, "123"}], []}} = Opt(["--foo", "123"]),
    {ok, {[{foo, "123"}], []}} = Opt(["--foo=123"]),
    {ok, {[{foo, ""}],    []}} = Opt(["-F"]),
    {ok, {[{foo, "123"}], []}} = Opt(["-F", "123"]),
    {ok, {[{foo, "123"}], []}} = Opt(["-F123"]),

    %% Built-in --help option
    {ok, print_help}             = NP(["--help"]),
    {ok, print_help}             = NP(["--help", "-F"]),
    {error, {unknown_opt, "-F"}} = NP(["-F", "--help"]),

    %% Built-in --version option (only when version is defined) (VP)
    {error, {unknown_opt, "--version"}} = NP(["--version"]),
    VP = fun(Args) -> element(1, parse_args(Args, [], [{version, "1.0"}])) end,
    {ok, print_version}          = VP(["--version"]),
    {ok, print_version}          = VP(["--version", "-F"]),
    {error, {unknown_opt, "-F"}} = VP(["-F", "--version"]),

    %% Kitchen sink (KS)
    KSOpts =
        [{flag, "-F, --flag", "", [flag]},
         {required, "--req", "", []},
         {optional, "-O", "", [optional_arg]}],
    KS = fun(Args) -> P(Args, KSOpts) end,
    {ok, {[flag, {required, "abc"}, {optional, "arg1"}], ["arg2"]}} =
        KS(["-F", "--req", "abc", "-O", "arg1", "arg2"]),
    {error, {missing_opt_arg, required, "--req"}} =
        KS(["-F", "--req", "-O", "arg1", "arg2"]),
    {ok, {[flag, {required, "abc"}, {optional, ""}], ["arg1", "arg2"]}} =
        KS(["-F", "--req", "abc", "-O", "--", "arg1", "arg2"]),

    %% Multiple values
    {ok, {[flag, flag, flag, {optional, "foo"}, {optional, "bar"}], []}} =
        KS(["-FFF", "-Ofoo", "-Obar"]),

    io:format("OK~n").

test_parse_pos_args() ->
    io:format("parse_pos_args: "),

    P = fun(Args, Pos) ->
                element(1, parse_args(Args, [], [{pos_args, Pos}]))
        end,

    {ok, {[], []}}                     = P([],         any),
    {ok, {[], ["a"]}}                  = P(["a"],      any),
    {ok, {[], ["a", "b"]}}             = P(["a", "b"], any),

    {ok, {[], ["a"]}}                  = P(["a"],      1),
    {error, missing_pos_arg}           = P([],         1),
    {error, {unexpected_pos_arg, "b"}} = P(["a", "b"], 1),

    {ok, {[], ["a", "b"]}}              = P(["a", "b"],     2),
    {error, missing_pos_arg}            = P([],              2),
    {error, missing_pos_arg}            = P(["a"],           2),
    {error, {unexpected_pos_arg, "c"}}  = P(["a", "b", "c"], 2),

    {ok, {[], ["a"]}}                   = P(["a"],           {1, 2}),
    {ok, {[], ["a", "b"]}}              = P(["a", "b"],      {1, 2}),
    {error, missing_pos_arg}            = P([],              {1, 2}),
    {error, {unexpected_pos_arg, "c"}}  = P(["a", "b", "c"], {1, 2}),

    {ok, {[], []}}                      = P([],              {any, 2}),
    {ok, {[], ["a"]}}                   = P(["a"],           {any, 2}),
    {ok, {[], ["a", "b"]}}              = P(["a", "b"],      {any, 2}),
    {error, {unexpected_pos_arg, "c"}}  = P(["a", "b", "c"], {any, 2}),

    {ok, {[], ["a"]}}                   = P(["a"],           {1, any}),
    {ok, {[], ["a", "b"]}}              = P(["a", "b"],      {1, any}),
    {ok, {[], ["a", "b", "c"]}}         = P(["a", "b", "c"], {1, any}),
    {error, missing_pos_arg}            = P([],              {1, any}),

    io:format("OK~n").

parse_args(Args, OptSpec) ->
    parse_args(Args, OptSpec, []).

parse_args(Args, OptSpec, Props) ->
    Parser = cli:parser("p", "", "", OptSpec, Props),
    cli:parse_args(Args, Parser).

test_opt_convert() ->
    io:format("opt_convert: "),

    1 = cli_opt:int_val(i, [{i, "1"}], 2, "i must be a number"),
    2 = cli_opt:int_val(i, [], 2, "i must be a number"),
    {error, "i must be a number"}
        = (catch cli_opt:int_val(i, [{i, "a"}], 2, "i must be a number")),

    io:format("OK~n").
