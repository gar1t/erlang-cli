-module(cli_parser).

-export([new/2, prog/1, usage/1, version/1, desc/1, options/1,
         parse_args/2]).

-record(parser, {prog, usage, version, desc, opts}).
-record(lo, {k, s, l, arg}). % lookup option (lo)

%% ===================================================================
%% New
%% ===================================================================

new(Prog, Opts) ->
    #parser{
       prog=Prog,
       usage=proplists:get_value(usage, Opts),
       version=proplists:get_value(version, Opts),
       desc=proplists:get_value(desc, Opts),
       opts=proplists:get_value(options, Opts, [])}.

%% ===================================================================
%% Attrs
%% ===================================================================

prog(#parser{prog=Prog}) -> Prog.

usage(#parser{usage=Usage}) -> Usage.

version(#parser{version=Version}) -> Version.

desc(#parser{desc=Desc}) -> Desc.

options(#parser{opts=Opts}) -> Opts.

%% ===================================================================
%% Parse args
%% ===================================================================

parse_args(Args, Parser) ->
    finalize_parsed(parse_tokens(tokenize(Args), Parser)).

tokenize(Args) ->
    acc_tokens(Args, has_opts, []).

acc_tokens([Arg|Rest], all_args, Acc) ->
    acc_tokens(Rest, all_args, [{arg, Arg}|Acc]);
acc_tokens(["--"|Rest], has_opts, Acc) ->
    acc_tokens(Rest, all_args, [argsep|Acc]);
acc_tokens(["--"++Long|Rest], has_opts, Acc) ->
    acc_tokens(Rest, has_opts, [long_opt(Long)|Acc]);
acc_tokens(["-"++Short|Rest], has_opts, Acc) ->
    acc_tokens(Rest, has_opts, [short_opt(Short)|Acc]);
acc_tokens([Arg|Rest], has_opts, Acc) ->
    acc_tokens(Rest, has_opts, [{arg, Arg}|Acc]);
acc_tokens([], _, Acc) ->
    lists:reverse(Acc).

long_opt(Opt) ->
    case re:split(Opt, "=", [{return, list}, {parts, 2}]) of
        [Name] -> {long, Name};
        [Name, Val] -> {long, Name, Val}
    end.

short_opt([Char]) -> {short, [Char]};
short_opt([Char|MoreChars]) -> {short, [Char], MoreChars}.

parse_tokens(Tokens, #parser{opts=Opts}) ->
    parse_tokens_acc(Tokens, opts_lookup(Opts), []).

opts_lookup(Opts) ->
    [lookup_opt(Opt) || Opt <- Opts].

lookup_opt(Opt) ->
    #lo{
       k=cli_opt:key(Opt),
       s=strip_short(cli_opt:short(Opt)),
       l=strip_long(cli_opt:long(Opt)),
       arg=lo_arg(Opt)
      }.

strip_short(undefined)  -> undefined;
strip_short("-"++Short) -> Short.

strip_long(undefined)  -> undefined;
strip_long("--"++Long) -> Long.

lo_arg(Opt) ->
    case cli_opt:has_arg(Opt) of
        yes      -> yes;
        optional -> opt;
        no       -> no
    end.

parse_tokens_acc([], _Lookup, Acc) ->
    {ok, lists:reverse(Acc)};
parse_tokens_acc(Tokens, Lookup, Acc) ->
    handle_try_parse(try_parse(Tokens, Lookup), Lookup, Acc).

handle_try_parse({ok, {Parsed, NextTokens}}, Lookup, Acc) ->
    parse_tokens_acc(NextTokens, Lookup, [Parsed|Acc]);
handle_try_parse({ok, NextTokens}, Lookup, Acc) ->
    parse_tokens_acc(NextTokens, Lookup, Acc);
handle_try_parse({error, Err}, _Lookup, _Acc) ->
    {error, Err}.

try_parse(Tokens, [LookupArg|RestLookup]) ->
    handle_opt(opt(Tokens, LookupArg), Tokens, RestLookup);
try_parse([{arg, Arg}|Rest], []) ->
    {ok, {{pos, Arg}, Rest}};
try_parse([OptToken|_], []) ->
    {error, {unknown_opt, opt_token_str(OptToken)}}.

handle_opt(nomatch, Tokens, Lookup) ->
    try_parse(Tokens, Lookup);
handle_opt({ok, {Opt, NextTokens}}, _, _) ->
    {ok, {{opt, Opt}, NextTokens}};
handle_opt({skipped, NextTokens}, _, _) ->
    {ok, NextTokens};
handle_opt({error, Err}, _, _) ->
    {error, Err}.

%% Long option - requires arg
opt([{long, Name, Val}|Rest], #lo{l=Name, arg=yes, k=Key}) ->
    {ok, {{Key, Val}, Rest}};
opt([{long, Name}, {arg, Val}|Rest], #lo{l=Name, arg=yes, k=Key}) ->
    {ok, {{Key, Val}, Rest}};
opt([{long, Name}|_], #lo{l=Name, arg=yes, k=Key}) ->
    {error, {missing_arg, Key, opt_token_str({long, Name})}};
%% Long option - takes no arg
opt([{long, Name}|Rest], #lo{l=Name, arg=no, k=Key}) ->
    {ok, {{Key, undefined}, Rest}};
opt([{long, Name, _Val}|_], #lo{l=Name, arg=no, k=Key}) ->
    {error, {unexpected_arg, Key, opt_token_str({long, Name})}};
%% Long option - might have arg
opt([{long, Name, Val}|Rest], #lo{l=Name, arg=opt, k=Key}) ->
    {ok, {{Key, Val}, Rest}};
opt([{long, Name}, {arg, Val}|Rest], #lo{l=Name, arg=opt, k=Key}) ->
    {ok, {{Key, Val}, Rest}};
opt([{long, Name}|Rest], #lo{l=Name, arg=opt, k=Key}) ->
    {ok, {{Key, ""}, Rest}};
%% Short option - requires arg
opt([{short, Name, Val}|Rest], #lo{s=Name, arg=yes, k=Key}) ->
    {ok, {{Key, Val}, Rest}};
opt([{short, Name}, {arg, Val}|Rest], #lo{s=Name, arg=yes, k=Key}) ->
    {ok, {{Key, Val}, Rest}};
opt([{short, Name}|_], #lo{s=Name, arg=yes, k=Key}) ->
    {error, {missing_arg, Key, opt_token_str({short, Name})}};
%% Short option - takes no arg
opt([{short, Name, MoreChars}|Rest], #lo{s=Name, arg=no, k=Key}) ->
    {ok, {{Key, undefined}, apply_more_short_chars(MoreChars, Rest)}};
opt([{short, Name}|Rest], #lo{s=Name, arg=no, k=Key}) ->
    {ok, {{Key, undefined}, Rest}};
%% Short option - might have arg
opt([{short, Name, Val}|Rest], #lo{s=Name, arg=opt, k=Key}) ->
    {ok, {{Key, Val}, Rest}};
opt([{short, Name}, {arg, Arg}|Rest], #lo{s=Name, arg=opt, k=Key}) ->
    {ok, {{Key, Arg}, Rest}};
opt([{short, Name}|Rest], #lo{s=Name, arg=opt, k=Key}) ->
    {ok, {{Key, ""}, Rest}};
opt([argsep|Rest], _) ->
    {skipped, Rest};
%% Other cases
opt(_, _) ->
    nomatch.

apply_more_short_chars([Char], Tokens) ->
    [{short, [Char]}|Tokens];
apply_more_short_chars([Char|Rest], Tokens) ->
    [{short, [Char], Rest}|Tokens].

finalize_parsed({ok, Parsed}) ->
    finalize_parsed_acc(Parsed, [], []);
finalize_parsed({error, {unknown_opt, "--help"}}) ->
    {ok, print_help};
finalize_parsed({error, {unknown_opt, "--version"}}) ->
    {ok, print_version};
finalize_parsed({error, Err}) ->
    {error, Err}.

finalize_parsed_acc([{opt, {Key, undefined}}|Rest], Opts, PosArgs) ->
    finalize_parsed_acc(Rest, [Key|Opts], PosArgs);
finalize_parsed_acc([{opt, {Key, Val}}|Rest], Opts, PosArgs) ->
    finalize_parsed_acc(Rest, [{Key, Val}|Opts], PosArgs);
finalize_parsed_acc([{pos, Arg}|Rest], Opts, PosArgs) ->
    finalize_parsed_acc(Rest, Opts, [Arg|PosArgs]);
finalize_parsed_acc([], Opts, PosArgs) ->
    {ok, {lists:reverse(Opts), lists:reverse(PosArgs)}}.

opt_token_str({long, Name})  -> "--" ++ Name;
opt_token_str({short, Name}) -> "-" ++ Name;
opt_token_str({short, Name, _}) -> "-" ++ Name.
