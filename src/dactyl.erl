%% Copyright (c) 2012 by Jeffrey Massung
%% All rights reserved
%%
%% No warranties explicit or implied. Use as you see fit for any projects
%% what-so-ever, but please give credit where due and leave this header
%% intact.

-module(dactyl).

%% ------------------------------------------------------------------
%% application Function Exports
%% ------------------------------------------------------------------

-export([compile/1]).
-export([compile_file/1]).
-export([new_template/1]).
-export([render/2]).
-export([render/3]).
-export([render_file/2]).
-export([render_file/3]).

-ignore_xref(compile/1).
-ignore_xref(compile_file/1).
-ignore_xref(new_template/1).
-ignore_xref(render/2).
-ignore_xref(render/3).
-ignore_xref(render_file/2).
-ignore_xref(render_file/3).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

%% Valid operations:
%% - literal: the argument should be inserted verbatim
%% - basic: a simple substitution
-type operation() :: literal | basic.
-export_type([operation/0]).

-type arg() :: any().
-export_type([arg/0]).
-type args() :: [arg()].
-export_type([args/0]).
-type segment() :: {operation(),args()}.
-export_type([segment/0]).

%% A #dactyl_template{} is a list of segments. Each segment is an operation
%% to create the output binary string. Most of the time, this will simply
%% be a {literal,[String]} command. The segments are in reverse order so
%% that building the final output string is done backwards, and quicker.
-record(dactyl_template,
        {segs = [] :: [segment()]
        }).
-type dactyl_template() :: #dactyl_template{}.
-export_type([dactyl_template/0]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%% @doc build a #dactyl_template{} from a source string or binary
compile (String) when is_list(String) ->
    compile(list_to_binary(String));
compile (Binary) when is_binary(Binary) ->
    make(#dactyl_template{},Binary,[]).

%% @doc build a #dactyl_template{} from a source file
compile_file (Filename) ->
    case file:read_file(Filename) of
        {ok,Binary} -> compile(Binary);
        Else -> Else
    end.

%% @doc Build an internal representation of a dactyl template
new_template(Segs) ->
    #dactyl_template{ segs = Segs }.

%% @doc render from a template
render (#dactyl_template{segs=Segs},Args) ->
    lists:flatten(lists:map(fun (Op) -> explode(Op,Args) end,Segs));

%% @doc compile the source, build a template, then render it
render (String,Args) when is_list(String) ->
    render(list_to_binary(String),Args);
render (Binary,Args) when is_binary(Binary) ->
    case compile(Binary) of
        {ok,Template} -> render(Template,Args);
        Else -> Else
    end.

%% @doc render from a template using module callbacks
render (Mod,#dactyl_template{segs=Segs},Args) ->
    lists:flatten(lists:map(fun (Op) -> explode(Op,{Mod,Args}) end,Segs));

%% @doc render with module callbacks
render (Mod,String,Args) when is_list(String) ->
    render(Mod,list_to_binary(String),Args);
render (Mod,Binary,Args) when is_binary(Binary) ->
    case compile(Binary) of
        {ok,Template} -> render(Mod,Template,Args);
        Else -> Else
    end.

%% @doc compile a source file, build a template, then render it
render_file (Filename,Args) ->
    case compile_file(Filename) of
        {ok,Template} -> render(Template,Args);
        Else -> Else
    end.

%% @doc compile a source file, build a template, then render it with callbacks
render_file (Mod,Filename,Args) ->
    case compile_file(Filename) of
        {ok,Template} -> render(Mod,Template,Args);
        Else -> Else
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

f (Fmt,Args) ->
    lists:flatten(io_lib:format(Fmt,Args)).

to_s (X) when is_list(X) -> X;
to_s (X) when is_binary(X) -> binary_to_list(X);
to_s (X) when is_atom(X) -> atom_to_list(X);
to_s (X) -> f("~p",[X]).

explode ({literal,[String]},_Args) ->
    to_s(String);
explode ({basic,[Param]},Args) ->
    to_s(lookup(Param,Args));
explode ({either,[Param,True,False]},Args) ->
    case lookup(Param,Args) of
        true -> render(True,Args);
        _ -> render(False,Args)
    end;
explode ({list,[Param,Template]},{Mod,_}=ModArgs) ->
    List=lookup(Param,ModArgs),
    [render(Mod,Template,Props) || Props <- List];
explode ({list,[Param,Template]},Args) ->
    List=lookup(Param,Args),
    [render(Template,Props) || Props <- List];
explode ({format,[Param,Fmt]},Args) ->
    List=lookup(Param,Args),
    io_lib:format(Fmt,List).

lookup (Param,{Mod,Args}) ->
    case lists:member({Param,1},Mod:module_info(exports)) of
        false -> lookup(Param,Args);
        true -> Mod:Param(Args)
    end;
lookup (Param,Args) ->
    proplists:get_value(Param,Args).


make (#dactyl_template{segs=Segs}=Template,<<>>,[]) ->
    {ok,Template#dactyl_template{segs=lists:reverse(Segs)}};
make (_,<<>>,_) ->
    {error,unexpected_end_of_template};
make (#dactyl_template{segs=Segs}=Template,<<$~,Binary/binary>>,TS) ->
    <<C,Tail/binary>>=Binary,
    case lists:member(C,TS) of
        true ->
            {ok,C,Template#dactyl_template{segs=lists:reverse(Segs)},Tail};
        false ->
            case format(Binary) of
                {ok,Op,XS,Rest} ->
                    make(Template#dactyl_template{segs=[{Op,XS}|Segs]},Rest,TS);
                Else ->
                    Else
            end
    end;
make (#dactyl_template{segs=Segs}=Template,Binary,TS) ->
    {ok,S,Rest} = literal(Binary,[]),
    make(Template#dactyl_template{segs=[{literal,[S]}|Segs]},Rest,TS).

literal (<<$~,$~,Rest/binary>>,S) ->
    case literal(Rest,S) of
        {ok,S,Binary} -> {ok,[$~|S],Binary};
        Else -> Else
    end;
literal (<<$~,_/binary>>=Rest,S) ->
    {ok,S,Rest};
literal (<<C,Rest/binary>>,S) ->
    {ok,S2,Binary} = literal(Rest,S),
    {ok,[C|S2],Binary};
literal (<<>>,S) ->
    {ok,S,<<>>}.

format (Binary) ->
    case scan(Binary) of
        {ok,Param,Term,Rest} ->
            case term_op(Term) of
                {ok,Op} -> Op(list_to_atom(Param),Rest);
                Else -> Else
            end;
        Else -> Else
    end.

scan (<<>>) ->
    {error,unexpected_end_of_template};
scan (<<$~,$~,Rest/binary>>) ->
    case scan(Rest) of
        {ok,S,Term,Tail} -> {ok,[$~|S],Term,Tail};
        Else -> Else
    end;
scan (<<$~,Term,Rest/binary>>) ->
    {ok,[],Term,Rest};
scan (<<C,Rest/binary>>) ->
    case scan(Rest) of
        {ok,S,Term,Tail} -> {ok,[C|S],Term,Tail};
        Else -> Else
    end.

term_op ($;) -> {ok,fun basic/2};
term_op ($?) -> {ok,fun either/2};
term_op ($[) -> {ok,fun list/2};
term_op (${) -> {ok,fun format/2};
term_op (Op) -> {error,{invalid_term_op,Op}}.

basic (Param,Rest) ->
    {ok,basic,[Param],Rest}.

either (Param,Binary) ->
    case make(#dactyl_template{},Binary,[$:,$;]) of
        {ok,$;,True,Rest} ->
            {ok,either,[Param,True,#dactyl_template{}],Rest};
        {ok,$:,True,Rest} ->
            case make(#dactyl_template{},Rest,[$;]) of
                {ok,_,False,Tail} ->
                    {ok,either,[Param,True,False],Tail};
                Else ->
                    Else
            end;
        Else ->
            Else
    end.

list (Param,Binary) ->
    case make(#dactyl_template{},Binary,[$]]) of
        {ok,_,Template,Rest} -> {ok,list,[Param,Template],Rest};
        Else -> Else
    end.

format (Param,Binary) ->
    case scan(Binary) of
        {ok,S,$},Rest} ->
            {ok,format,[Param,S],Rest};
        {ok,S,Fmt,Rest} ->
            case format(Param,Rest) of
                {ok,format,[_,S2],Tail} ->
                    {ok,format,[Param,lists:flatten([S,[$~,Fmt],S2])],Tail};
                Else ->
                    Else
            end;
        Else ->
            Else
    end.
