-module(cli_main).

-export([main/3]).

-define(ok_exit, 0).
-define(error_exit, 1).
-define(code(N), is_integer(N)).

main(Args, Parser, Handler) ->
    handle_main_parse_args(cli:parse_args(Args, Parser), Handler).

handle_main_parse_args({{ok, print_help}, P}, _) ->
    print_help(P);
handle_main_parse_args({{ok, print_version}, P}, _) ->
    print_version(P);
handle_main_parse_args({{ok, Parsed}, _}, Handle) ->
    handle_parsed(Parsed, Handle);
handle_main_parse_args({{error, Err}, P}, _) ->
    print_error(Err, P).

print_help(P) ->
    cli:print_help(P), ?ok_exit.

print_version(P) ->
    cli:print_version(P), ?ok_exit.

print_error(Err, P) ->
    cli:print_error(Err, P), ?error_exit.

handle_parsed(Parsed, Handle) ->
    result_to_code(Handle(Parsed)).

result_to_code(ok)                       -> ?ok_exit;
result_to_code({ok, N}) when ?code(N)    -> N;
result_to_code(error)                    -> ?error_exit;
result_to_code({error, N}) when ?code(N) -> N.
