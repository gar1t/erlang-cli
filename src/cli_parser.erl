-module(cli_parser).

-export([new/4, prog/1, version/1, desc/1,
         args/1, options/1, positional/1]).

-export([parse_args/2]).

-record(parser, {prog, version, desc, args}).
-record(scan, {short_circuit, args}).

%% ===================================================================
%% New
%% ===================================================================

new(Program, Version, Desc, Args) ->
    #parser{prog=Program, version=Version, desc=Desc, args=Args}.

%% ===================================================================
%% Attrs
%% ===================================================================

prog(#parser{prog=Prog}) -> Prog.

version(#parser{version=Version}) -> Version.

desc(#parser{desc=Desc}) -> Desc.

args(#parser{args=Args}) -> Args.

options(Parser) ->
    [Arg || Arg <- args(Parser), cli_arg:arg_type(Arg) == option].

positional(Parser) ->
    [Arg || Arg <- args(Parser), cli_arg:arg_type(Arg) == positional].

%% ===================================================================
%% Parse args
%% ===================================================================

parse_args(Args, Parser) ->
    Scanned = scan_args(Args, init_scan(Parser)),
    eval_scanned(Scanned, Parser).

init_scan(_Parser) ->
    #scan{args=[]}.

scan_args([Arg|Rest], Scan) ->
    handle_scan(scan_arg(Arg, Scan), Rest);
scan_args([], Scan) ->
    Scan.

handle_scan({stop,     Last}, _Rest) -> Last;
handle_scan({continue, Next},  Rest) -> scan_args(Rest, Next).

scan_arg("--help", S) ->
    {stop, S#scan{short_circuit=print_help}};
scan_arg("--version", S) ->
    {stop, S#scan{short_circuit=print_version}}.

eval_scanned(#scan{short_circuit=undefined, args=Args}, _Parser) ->
    {ok, Args};
eval_scanned(#scan{short_circuit=ShortCircuit}, _Parser) ->
    {ok, ShortCircuit}.
