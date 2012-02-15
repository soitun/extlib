-module(erlopt).

-author("gerald.gutierrez@gmail.com").

-export([getopt/2]).

% -------------------------------------------------------------------------
% API

getopt(Spec, Args) -> idle(Spec, elements(Args), []).

% -------------------------------------------------------------------------
% Internal functions

elements(Args) -> lists:foldr(fun(E, A) -> E ++ [ee] ++ A end, [el], Args).

option_type(Spec, short, Opt)  -> option_type(Spec, 1, Opt);
option_type(Spec, long, Opt)   -> option_type(Spec, 2, Opt);
option_type(Spec, Column, Opt) -> 
    case lists:keyfind(Opt, Column, Spec) of
        false        -> throw({erlopt, bad_option, Opt});
        {_, _, Type} -> Type
    end.

short_option(Spec, [H|T], Acc) ->
    case option_type(Spec, short, [H]) of
        no    -> osn1(Spec, T, [{opt, {[H], []}}|Acc]);
        maybe -> osm1(Spec, T, Acc, {H, []});
        yes   -> osy1(Spec, T, Acc, {H, []})
    end.

stop(Acc)                         -> lists:reverse(Acc).

idle(_, [el|_], Acc)              -> stop(Acc);
idle(Spec, [$-|T], Acc)           -> dsh1(Spec, T, Acc);
idle(Spec, [H|T], Acc)            -> arg1(Spec, T, Acc, [H]).

arg1(Spec, [ee|T], Acc, S)        -> idle(Spec, T, [{arg, lists:reverse(S)}|Acc]);
arg1(Spec, [H|T], Acc, S)         -> arg1(Spec, T, Acc, [H|S]).

dsh1(Spec, [$-|T], Acc)           -> dsh2(Spec, T, Acc);
dsh1(Spec, [ee|T], Acc)           -> idle(Spec, T, [{arg, "-"}|Acc]);
dsh1(Spec, Elems, Acc)            -> short_option(Spec, Elems, Acc).

dsh2(_, [ee|T], Acc)              -> arg2(T, Acc, []);
dsh2(Spec, [H|T], Acc)            -> oln1(Spec, T, Acc, {[H], []}).

arg2([el|_], Acc, _)              -> stop(Acc);
arg2([ee|T], Acc, S)              -> arg2(T, [{arg, lists:reverse(S)}|Acc], []);
arg2([H|T], Acc, S)               -> arg2(T, Acc, [H|S]).

osn1(Spec, [ee|T], Acc)           -> idle(Spec, T, Acc);
osn1(Spec, Elems, Acc)            -> short_option(Spec, Elems, Acc).

osm1(Spec, [ee|T], Acc, {Opt, S}) -> idle(Spec, T, [{opt, {[Opt], lists:reverse(S)}}|Acc]);
osm1(Spec, [H|T], Acc, {Opt, S})  -> osm1(Spec, T, Acc, {Opt, [H|S]}).

osy1(Spec, [ee|T], Acc, {Opt, S}) -> osy2(Spec, T, Acc, {Opt, S});
osy1(Spec, [H|T], Acc, {Opt, S})  -> osy3(Spec, T, Acc, {Opt, [H|S]}).

osy2(_, [el|_], _, {Opt, _})      -> throw({erlopt, option_requires_argument, [Opt]});
osy2(Spec, [ee|T], Acc, {Opt, S}) -> idle(Spec, T, [{opt, {[Opt], lists:reverse(S)}}|Acc]);
osy2(Spec, [H|T], Acc, {Opt, S})  -> osy2(Spec, T, Acc, {Opt, [H|S]}).

osy3(Spec, [ee|T], Acc, {Opt, S}) -> idle(Spec, T, [{opt, {[Opt], lists:reverse(S)}}|Acc]);
osy3(Spec, [H|T], Acc, {Opt, S})  -> osy2(Spec, T, Acc, {Opt, [H|S]}).

oln1(Spec, [ee|T], Acc, {Opt, S}) ->
    Opt2 = lists:reverse(Opt),
    case option_type(Spec, long, Opt2) of
        no    -> idle(Spec, T, [{opt, {Opt2, lists:reverse(S)}}|Acc]);
        maybe -> idle(Spec, T, [{opt, {Opt2, lists:reverse(S)}}|Acc]);
        yes   -> oln2(Spec, T, Acc, {Opt2, S})
    end;

oln1(Spec, [$=|T], Acc, {Opt, S}) ->
    Opt2 = lists:reverse(Opt),
    case option_type(Spec, long, Opt2) of
        no    -> throw({erlopt, option_requires_no_argument, Opt2});
        maybe -> oln3(Spec, T, Acc, {Opt2, S});
        yes   -> oln3(Spec, T, Acc, {Opt2, S})
    end;

oln1(Spec, [H|T], Acc, {Opt, S})  -> oln1(Spec, T, Acc, {[H|Opt], S}).

oln2(_, [el|_], _, {Opt, _})      -> throw({erlopt, option_requires_argument, Opt});
oln2(Spec, [ee|T], Acc, {Opt, S}) -> idle(Spec, T, [{opt, {Opt, lists:reverse(S)}}|Acc]);
oln2(Spec, [H|T], Acc, {Opt, S})  -> oln2(Spec, T, Acc, {Opt, [H|S]}).

oln3(Spec, [ee|T], Acc, {Opt, S}) -> idle(Spec, T, [{opt, {Opt, lists:reverse(S)}}|Acc]);
oln3(Spec, [H|T], Acc, {Opt, S})  -> oln2(Spec, T, Acc, {Opt, [H|S]}).

