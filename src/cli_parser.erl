-module(cli_parser).

-export([new/1, usage/1, version/1, desc/1, options/1]).

-record(parser, {usage, version, desc, opts}).

%% ===================================================================
%% New
%% ===================================================================

new(Opts) ->
    #parser{
       usage=proplists:get_value(usage, Opts),
       version=proplists:get_value(version, Opts),
       desc=proplists:get_value(desc, Opts),
       opts=proplists:get_value(options, Opts, [])}.

%% ===================================================================
%% Attrs
%% ===================================================================

usage(#parser{usage=Usage}) -> Usage.

version(#parser{version=Version}) -> Version.

desc(#parser{desc=Desc}) -> Desc.

options(#parser{opts=Opts}) -> Opts.
