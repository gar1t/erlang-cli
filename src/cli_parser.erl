-module(cli_parser).

-export([new/2, prog/1, usage/1, version/1, desc/1, options/1,
         commands/1, is_command_parser/1, parse_args/2]).

-record(parser, {prog, usage, version, desc, opts, cmds, pos_args}).
-record(ps, {p, opts, mode}). % parse state (ps)
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
       opts=proplists:get_value(options, Opts, []),
       cmds=proplists:get_value(commands, Opts, []),
       pos_args=proplists:get_value(pos_args, Opts, any)}.

%% ===================================================================
%% Attrs
%% ===================================================================

prog(#parser{prog=Prog}) -> Prog.

usage(#parser{usage=Usage}) -> Usage.

version(#parser{version=Version}) -> Version.

desc(#parser{desc=Desc}) -> Desc.

options(#parser{opts=Opts}) -> Opts.

commands(#parser{cmds=Cmds}) -> Cmds.

is_command_parser(#parser{cmds=Cmds}) -> length(Cmds) > 0.

%% ===================================================================
%% Parse args
%% ===================================================================

parse_args(Args, RootParser) ->
    Tokens = tokenize(Args),
    {ParseResult, ActiveParser} = parse_tokens(Tokens, RootParser),
    finalize(ParseResult, ActiveParser).

%% -------------------------------------------------------------------
%% Tokenize
%% -------------------------------------------------------------------

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
        [Name]      -> {long, Name, undefined};
        [Name, Val] -> {long, Name, Val}
    end.

short_opt([Char])           -> {short, [Char], undefined};
short_opt([Char|MoreChars]) -> {short, [Char], MoreChars}.

%% -------------------------------------------------------------------
%% Parse
%% -------------------------------------------------------------------

parse_tokens(Tokens, Parser) ->
    case is_command_parser(Parser) of
        true  -> parse_command_tokens(Tokens, Parser);
        false -> parse_all_tokens(Tokens, Parser)
    end.

parse_command_tokens(Tokens, Parser) ->
    PS = parse_state(Parser, to_pos),
    handle_parse_command_tokens(parse_tokens_acc(Tokens, PS, []), PS).

handle_parse_command_tokens({ok, {Cmd, RestTokens, Parsed}}, PS) ->
    try_sub_parse_tokens(
      find_sub_parser(Cmd, PS),
      Cmd, RestTokens, Parsed, PS);
handle_parse_command_tokens({ok, _Parsed}, #ps{p=Parser}) ->
    {{error, missing_command}, Parser};
handle_parse_command_tokens({error, Err}, #ps{p=Parser}) ->
    {{error, Err}, Parser}.

find_sub_parser(Cmd, #ps{p=#parser{cmds=Cmds}}) ->
    case lists:keyfind(Cmd, 1, Cmds) of
        {_, _, Parser} -> {ok, Parser};
        false -> error
    end.

try_sub_parse_tokens({ok, Parser}, Cmd, RestTokens, Parsed, _PS0) ->
    PS = parse_state(Parser, all),
    SubParseResult = sub_parse_tokens(Cmd, RestTokens, Parsed, PS),
    {SubParseResult, Parser};
try_sub_parse_tokens(error, Cmd, _RestTokens, _Parsed, #ps{p=Parser}) ->
    SubParseResult = {error, {unknown_command, Cmd}},
    {SubParseResult, Parser}.

sub_parse_tokens(Cmd, Tokens, Parsed, PS) ->
    handle_sub_parse_tokens(parse_tokens_acc(Tokens, PS, Parsed), Cmd).

handle_sub_parse_tokens({ok, Parsed}, Cmd) ->
    {ok, {Cmd, Parsed}};
handle_sub_parse_tokens({error, Err}, _Cmd) ->
    {error, Err}.

parse_all_tokens(Tokens, Parser) ->
    PS = parse_state(Parser, all),
    ParseResult = parse_tokens_acc(Tokens, PS, []),
    {ParseResult, Parser}.

parse_tokens_acc([], _PS, Acc) ->
    {ok, lists:reverse(Acc)};
parse_tokens_acc(Tokens, #ps{opts=Opts}=PS, Acc) ->
    handle_try_parse(try_parse(Tokens, Opts), PS, Acc).

handle_try_parse({ok, {{pos, Arg}, NextTokens}}, #ps{mode=to_pos}, Acc) ->
    {ok, {Arg, NextTokens, lists:reverse(Acc)}};
handle_try_parse({ok, {Parsed, NextTokens}}, PS, Acc) ->
    parse_tokens_acc(NextTokens, PS, [Parsed|Acc]);
handle_try_parse({ok, NextTokens}, PS, Acc) ->
    parse_tokens_acc(NextTokens, PS, Acc);
handle_try_parse({error, Err}, _PS, _Acc) ->
    {error, Err}.

try_parse(Tokens, [LookupOpt|RestLookup]) ->
    handle_opt(opt(Tokens, LookupOpt), Tokens, RestLookup);
try_parse([{arg, Arg}|RestTokens], []) ->
    {ok, {{pos, Arg}, RestTokens}};
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

opt([{long, Name, Val}|Rest], #lo{l=Name}=LO) ->
    long_opt({Name, Val}, Rest, LO);
opt([{short, Name, MoreChars}|Rest], #lo{s=Name}=LO) ->
    short_opt({Name, MoreChars}, Rest, LO);
opt([argsep|Rest], _) ->
    {skipped, Rest};
opt(_, _) ->
    nomatch.

long_opt(Token, Rest, #lo{arg=yes, k=Key}) ->
    long_opt_required_arg(Token, Rest, Key);
long_opt(Token, Rest, #lo{arg=no, k=Key}) ->
    long_opt_no_arg(Token, Rest, Key);
long_opt(Token, Rest, #lo{arg=opt, k=Key}) ->
    long_opt_maybe_arg(Token, Rest, Key).

long_opt_required_arg({_Name, Val}, Rest, Key) when Val /= undefined ->
    {ok, {{Key, Val}, Rest}};
long_opt_required_arg({_Name, undefined}, [{arg, Val}|Rest], Key) ->
    {ok, {{Key, Val}, Rest}};
long_opt_required_arg({Name, undefined}, _Rest, Key) ->
    {error, {missing_opt_arg, Key, opt_token_str({long, Name})}};
long_opt_required_arg(_Token, _Rest, _Key) ->
    nomatch.

long_opt_no_arg({_Name, undefined}, Rest, Key) ->
    {ok, {{Key, undefined}, Rest}};
long_opt_no_arg({Name, _Val}, _Rest, Key) ->
    {error, {unexpected_opt_arg, Key, opt_token_str({long, Name})}};
long_opt_no_arg(_Token, _Rest, _Key) ->
    nomatch.

long_opt_maybe_arg({_Name, Val}, Rest, Key) when Val /= undefined ->
    {ok, {{Key, Val}, Rest}};
long_opt_maybe_arg({_Name, undefined}, [{arg, Val}|Rest], Key) ->
    {ok, {{Key, Val}, Rest}};
long_opt_maybe_arg({_Name, undefined}, Rest, Key) ->
    {ok, {{Key, ""}, Rest}};
long_opt_maybe_arg(_Token, _Rest, _Key) ->
    nomatch.

short_opt(Token, Rest, #lo{arg=yes, k=Key}) ->
    short_opt_required_arg(Token, Rest, Key);
short_opt(Token, Rest, #lo{arg=no, k=Key}) ->
    short_opt_no_arg(Token, Rest, Key);
short_opt(Token, Rest, #lo{arg=opt, k=Key}) ->
    short_opt_maybe_arg(Token, Rest, Key).

short_opt_required_arg({_Name, Val}, Rest, Key) when Val /= undefined ->
    {ok, {{Key, Val}, Rest}};
short_opt_required_arg({_Name, undefined}, [{arg, Val}|Rest], Key) ->
    {ok, {{Key, Val}, Rest}};
short_opt_required_arg({Name, undefined}, _Rest, Key) ->
    {error, {missing_opt_arg, Key, opt_token_str({short, Name})}};
short_opt_required_arg(_Token, _Rest, _Key) ->
    nomatch.

short_opt_no_arg({_Name, MoreChars}, Rest, Key) when MoreChars /= undefined ->
    {ok, {{Key, undefined}, apply_more_short_chars(MoreChars, Rest)}};
short_opt_no_arg({_Name, undefined}, Rest, Key) ->
    {ok, {{Key, undefined}, Rest}};
short_opt_no_arg(_Token, _Rest, _Key) ->
    nomatch.

apply_more_short_chars([Char], Tokens) ->
    [{short, [Char], undefined}|Tokens];
apply_more_short_chars([Char|Rest], Tokens) ->
    [{short, [Char], Rest}|Tokens].

short_opt_maybe_arg({_Name, Val}, Rest, Key) when Val /= undefined ->
    {ok, {{Key, Val}, Rest}};
short_opt_maybe_arg({_Name, undefined}, [{arg, Val}|Rest], Key) ->
    {ok, {{Key, Val}, Rest}};
short_opt_maybe_arg({_Name, undefined}, Rest, Key) ->
    {ok, {{Key, ""}, Rest}};
short_opt_maybe_arg(_Token, _Rest, _Key) ->
    nomatch.

opt_token_str({long,  Name})    -> "--" ++ Name;
opt_token_str({long,  Name, _}) -> "--" ++ Name;
opt_token_str({short, Name})    -> "-" ++ Name;
opt_token_str({short, Name, _}) -> "-" ++ Name.

%% -------------------------------------------------------------------
%% Parse state
%% -------------------------------------------------------------------

parse_state(Parser, Mode) ->
    #ps{p=Parser, opts=opts_lookup(Parser), mode=Mode}.

opts_lookup(#parser{opts=Opts}) ->
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

%% -------------------------------------------------------------------
%% Finalize
%% -------------------------------------------------------------------

finalize({ok, {Cmd, Parsed}}, Parser) ->
    {Opts, PosArgs} = finalize_acc(Parsed, [], []),
    handle_validate_pos_args(
      validate_pos_args(PosArgs, Parser),
      {Cmd, Opts, PosArgs}, Parser);

finalize({ok, Parsed}, Parser) ->
    {Opts, PosArgs} = finalize_acc(Parsed, [], []),
    handle_validate_pos_args(
      validate_pos_args(PosArgs, Parser),
      {Opts, PosArgs}, Parser);
finalize({error, {unknown_opt, "--help"}}, Parser) ->
    handle_unknown_help_opt(Parser);
finalize({error, {unknown_opt, "--version"}=Err}, Parser) ->
    handle_unknown_version_opt(Parser, Err);
finalize({error, Err}, Parser) ->
    {{error, Err}, Parser}.

validate_pos_args(Args, #parser{pos_args=Spec}) ->
    check_arg_len(Args, Spec).

check_arg_len(_, any)                 -> ok;
check_arg_len([], 0)                  -> ok;
check_arg_len([], {0, _})             -> ok;
check_arg_len([], {any, _})           -> ok;
check_arg_len([Arg|_], 0)             -> {error, {unexpected_pos_arg, Arg}};
check_arg_len([Arg|_], {_, 0})        -> {error, {unexpected_pos_arg, Arg}};
check_arg_len([], N) when N > 0       -> {error, missing_pos_arg};
check_arg_len([], {N, _}) when N > 0  -> {error, missing_pos_arg};
check_arg_len([_|Rest], N)            -> check_arg_len(Rest, decr_arg_pos(N)).

decr_arg_pos(any)                         -> any;
decr_arg_pos(N) when is_integer(N), N > 0 -> N - 1;
decr_arg_pos(N) when is_integer(N)        -> N;
decr_arg_pos({N, M})                      -> {decr_arg_pos(N), decr_arg_pos(M)}.

handle_validate_pos_args(ok, ParseResult, Parser) ->
    {{ok, ParseResult}, Parser};
handle_validate_pos_args({error, Err}, _ParseResult, Parser) ->
    {{error, Err}, Parser}.

handle_unknown_help_opt(Parser) ->
    {{ok, print_help}, Parser}.

handle_unknown_version_opt(#parser{version=undefined}=Parser, Err) ->
    {{error, Err}, Parser};
handle_unknown_version_opt(Parser, _Err) ->
    {{ok, print_version}, Parser}.

finalize_acc([{opt, {Key, undefined}}|Rest], Opts, PosArgs) ->
    finalize_acc(Rest, [Key|Opts], PosArgs);
finalize_acc([{opt, {Key, Val}}|Rest], Opts, PosArgs) ->
    finalize_acc(Rest, [{Key, Val}|Opts], PosArgs);
finalize_acc([{pos, Arg}|Rest], Opts, PosArgs) ->
    finalize_acc(Rest, Opts, [Arg|PosArgs]);
finalize_acc([], Opts, PosArgs) ->
    {lists:reverse(Opts), lists:reverse(PosArgs)}.
