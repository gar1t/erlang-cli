-module(cli_parser).

-export([new/2, prog/1, usage/1, version/1, desc/1, options/1,
         commands/1, is_command_parser/1, parse_args/2]).

-record(parser, {prog, usage, version, desc, opts, cmds}).
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
       cmds=proplists:get_value(commands, Opts, [])}.

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

parse_args(Args, Parser) ->
    Tokens = tokenize(Args),
    Parsed = parse_tokens(Tokens, Parser),
    finalize_parsed(Parsed, Parser).

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
        [Name] -> {long, Name};
        [Name, Val] -> {long, Name, Val}
    end.

short_opt([Char]) -> {short, [Char]};
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
    {error, missing_command, Parser};
handle_parse_command_tokens({error, Err, Parser}, _PS) ->
    {error, Err, Parser}.

find_sub_parser(Cmd, #ps{p=#parser{cmds=Cmds}}) ->
    case lists:keyfind(Cmd, 1, Cmds) of
        {_, _, Parser} -> {ok, Parser};
        false -> error
    end.

try_sub_parse_tokens({ok, Parser}, Cmd, RestTokens, Parsed, _PS0) ->
    PS = parse_state(Parser, all),
    handle_sub_parse_tokens(parse_tokens_acc(RestTokens, PS, Parsed), Cmd);
try_sub_parse_tokens(error, Cmd, _RestTokens, _Parsed, #ps{p=Parser}) ->
    {error, {unknown_command, Cmd}, Parser}.

handle_sub_parse_tokens({ok, Parsed}, Cmd) ->
    {ok, {Cmd, Parsed}};
handle_sub_parse_tokens({error, Err, Parser}, _Cmd) ->
    {error, Err, Parser}.

parse_all_tokens(Tokens, Parser) ->
    PS = parse_state(Parser, all),
    parse_tokens_acc(Tokens, PS, []).

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
handle_try_parse({error, Err}, #ps{p=Parser}, _Acc) ->
    {error, Err, Parser}.

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

opt_token_str({long, Name})  -> "--" ++ Name;
opt_token_str({short, Name}) -> "-" ++ Name;
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

finalize_parsed({ok, {Cmd, Parsed}}, _Root) ->
    {Opts, Pos} = finalize_parsed_acc(Parsed, [], []),
    {ok, {Cmd, Opts, Pos}};
finalize_parsed({ok, Parsed}, _Root) ->
    {ok, finalize_parsed_acc(Parsed, [], [])};
finalize_parsed({error, {unknown_opt, "--help"}, Parser}, _Root) ->
    handle_unknown_help_opt(Parser);
finalize_parsed({error, {unknown_opt, "--version"}=Err, Root}, Root) ->
    handle_unknown_version_opt(Root, Err);
finalize_parsed({error, Err, Parser}, _Root) ->
    {error, Err, Parser}.

handle_unknown_help_opt(Parser) ->
    {ok, {print_help, Parser}}.

handle_unknown_version_opt(#parser{version=undefined}=Parser, Err) ->
    {error, Err, Parser};
handle_unknown_version_opt(Parser, _Err) ->
    {ok, {print_version, Parser}}.

finalize_parsed_acc([{opt, {Key, undefined}}|Rest], Opts, PosArgs) ->
    finalize_parsed_acc(Rest, [Key|Opts], PosArgs);
finalize_parsed_acc([{opt, {Key, Val}}|Rest], Opts, PosArgs) ->
    finalize_parsed_acc(Rest, [{Key, Val}|Opts], PosArgs);
finalize_parsed_acc([{pos, Arg}|Rest], Opts, PosArgs) ->
    finalize_parsed_acc(Rest, Opts, [Arg|PosArgs]);
finalize_parsed_acc([], Opts, PosArgs) ->
    {lists:reverse(Opts), lists:reverse(PosArgs)}.
