-module(cli_tests).

-compile([nowarn_unused_function]).

-export([run/0]).

run() ->
    test_cli_opt(),
    test_parse_args().

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

    P = fun(Args, OptSpec) -> parse_args(Args, OptSpec) end,

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
    {error, {missing_arg, foo, "--foo"}} = Req(["--foo"]),
    {ok, {[{foo, "123"}], []}}           = Req(["--foo", "123"]),
    {ok, {[{foo, "123"}], []}}           = Req(["--foo=123"]),
    {error, {missing_arg, foo, "-F"}}    = Req(["-F"]),
    {ok, {[{foo, "123"}], []}}           = Req(["-F", "123"]),
    {ok, {[{foo, "123"}], []}}           = Req(["-F123"]),

    %% Option with no arg (NoArg)
    NoArg = fun(Args) -> P(Args, [{foo, "-F, --foo", "", [no_arg]}]) end,
    {ok, {[foo], []}}                       = NoArg(["--foo"]),
    {ok, {[foo], ["123"]}}                  = NoArg(["--foo", "123"]),
    {error, {unexpected_arg, foo, "--foo"}} = NoArg(["--foo=123"]),
    {ok, {[foo], []}}                       = NoArg(["-F"]),
    {ok, {[foo], ["123"]}}                  = NoArg(["-F", "123"]),
    {error, {unknown_opt, "-1"}}            = NoArg(["-F123"]),

    %% Option with optional arg (Opt)
    Opt = fun(Args) -> P(Args, [{foo, "-F, --foo", "", [optional_arg]}]) end,
    {ok, {[{foo, ""}],    []}} = Opt(["--foo"]),
    {ok, {[{foo, "123"}], []}} = Opt(["--foo", "123"]),
    {ok, {[{foo, "123"}], []}} = Opt(["--foo=123"]),
    {ok, {[{foo, ""}],    []}} = Opt(["-F"]),
    {ok, {[{foo, "123"}], []}} = Opt(["-F", "123"]),
    {ok, {[{foo, "123"}], []}} = Opt(["-F123"]),

    %% Special long options (help and version)
    {ok, print_help}             = NP(["--help"]),
    {ok, print_version}          = NP(["--version"]),
    {ok, print_help}             = NP(["--help", "-F"]),
    {ok, print_version}          = NP(["--version", "-F"]),
    {error, {unknown_opt, "-F"}} = NP(["-F", "--help"]),
    {error, {unknown_opt, "-F"}} = NP(["-F", "--version"]),

    %% Kitchen sink (KS)
    KSOpts =
        [{flag, "-F, --flag", "", [flag]},
         {required, "--req", "", []},
         {optional, "-O", "", [optional_arg]}],
    KS = fun(Args) -> P(Args, KSOpts) end,
    {ok, {[flag, {required, "abc"}, {optional, "arg1"}], ["arg2"]}} =
        KS(["-F", "--req", "abc", "-O", "arg1", "arg2"]),
    {error, {missing_arg, required, "--req"}} =
        KS(["-F", "--req", "-O", "arg1", "arg2"]),
    {ok, {[flag, {required, "abc"}, {optional, ""}], ["arg1", "arg2"]}} =
        KS(["-F", "--req", "abc", "-O", "--", "arg1", "arg2"]),

    %% Multiple values
    {ok, {[flag, flag, flag, {optional, "foo"}, {optional, "bar"}], []}} = 
        KS(["-FFF", "-Ofoo", "-Obar"]),

    io:format("OK~n").

parse_args(Args, OptSpec) ->
    Parser = cli:parser("p", "", "", OptSpec),
    cli:parse_args(Args, Parser).
