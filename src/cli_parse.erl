-module(cli_parse).

-export([parse_args/2]).

-record(scan, {short_circuit, args=[]}).

parse_args(Args, Parser) ->
    Scanned = scan_args(Args, init_scan(Parser)),
    eval_scanned(Scanned, Parser).

init_scan(_Parser) -> #scan{}.

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
