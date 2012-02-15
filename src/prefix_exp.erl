-module(prefix_exp).

-import(extbif, [to_list/1, to_integer/1]).

-import(lists, [foldl/3, keysearch/3]).

-export([is_digital/1, scan/1, parse/1, valid/1, eval/2]).

%%string -> tokens
scan(S) ->
    scan(to_list(S), [], []).

%return
scan([], [], Result) ->
    lists:reverse(Result);

scan([], Acc, Result) ->
    lists:reverse([lists:reverse(Acc) | Result]);

%tokens: & | !
scan([$\s | S] , [], Result) ->
    scan(S, [], Result);

scan([$\s | S], Acc, Result) ->
    scan(S, [], [lists:reverse(Acc) | Result]);

scan([$& | S], [], Result) ->
    scan(S, [], ['&' | Result]);

scan([$| | S], [], Result) ->
    scan(S, [], ['|' | Result]);

scan([$!, $= | S], [], Result) ->
    scan(S, [], ['!=' | Result]);

scan([$! | S], [], Result) ->
    scan(S, [], ['!' | Result]);

%tokens: > < = >= <=
scan([$( | S], [], Result) ->
    scan(S, [], ['(' | Result]);

scan([$) | S], [], Result) ->
    scan(S, [], [')' | Result]);

scan([$) | S], Acc, Result) ->
    scan(S, [], [')', lists:reverse(Acc) | Result]);

scan([$>, $= | S], [], Result) ->
    scan(S, [], ['>=' | Result]);

scan([$<, $= | S], [], Result) ->
    scan(S, [], ['<=' | Result]);

scan([$> | S], [], Result) ->
    scan(S, [], ['>' | Result]);

scan([$< | S], [], Result) ->
    scan(S, [], ['<' | Result]);

scan([$= | S], [], Result) ->
    scan(S, [], ['=' | Result]);

scan([C|S], Acc, Result) ->
    scan(S, [C | Acc], Result).
%%
%% Example: "| (> a 1) (>= b 2)"
%%
parse(S) ->
    Tokens = scan(to_list(S)),
    Exp = parse1(Tokens),
    valid(Exp),
    {ok, Exp}.

parse1(['&' | T]) ->
    {'&', parse1(T)};

parse1(['|' | T]) ->
    {'|', parse1(T)};

parse1(['!' | T]) ->
    {'!', parse1(T)};

parse1(['>', L, R]) ->
    {'>', list_to_atom(L), to_integer(R)};

parse1(['<', L, R]) ->
    {'<', list_to_atom(L), to_integer(R)};

parse1(['=', L, R]) ->
    case is_digital(R) of
    true ->
        {'=', list_to_atom(L), to_integer(R)};
    false ->
        {'=', list_to_atom(L), R}
    end;

parse1(['!=', L, R]) ->
    case is_digital(R) of
    true ->
        {'!=', list_to_atom(L), to_integer(R)};
    false ->
        {'!=', list_to_atom(L), R}
    end;

parse1(['>=', L, R]) ->
    {'>=', list_to_atom(L), to_integer(R)};

parse1(['<=', L, R]) ->
    {'<=', list_to_atom(L), to_integer(R)};

parse1(['(' | _] = Tokens) ->
    parse1(split(Tokens));

parse1([L | _] = Tokens) when is_list(L) ->
    [parse1(X) || X <- Tokens];

parse1(Exp) ->
    Exp.

split(Tokens) ->
    split(Tokens, 0, [], []).

split([], _, _, Result) ->
    Result;

split([H|T], Num, Rest, Result) ->
    NewNum = case H of
    '(' -> Num + 1;
    ')' -> Num - 1;
     _ -> Num
     end,
    if
	NewNum == 0 ->
	    X = Rest++[H],
	    LenX = length(X),
	    if
		LenX > 2 ->
		    split(T, 0, [], Result ++ [lists:sublist(X, 2, LenX-2)]);
		true ->
		    split(T, 0, Rest, Result)
	    end;
	true ->
	    split(T, NewNum, Rest++[H], Result)
    end.

valid({'|', Exps}) when is_list(Exps) ->
    [valid(Exp) || Exp <- Exps];

valid({'&', Exps}) when is_list(Exps) ->
    [valid(Exp) || Exp <- Exps];

valid({'!', Exp}) when is_tuple(Exp) ->
    valid(Exp);

valid({'>', Name, _Val}) when is_atom(Name) ->
    ok;

valid({'<', Name, _Val}) when is_atom(Name) ->
    ok;

valid({'!=', Name, _Val}) when is_atom(Name) ->
    ok;

valid({'=', Name, _Val}) when is_atom(Name) ->
    ok;

valid({'>=', Name, _Val}) when is_atom(Name) ->
    ok;

valid({'<=', Name, _Val}) when is_atom(Name) ->
    ok;

valid(Exp) ->
    throw({invalid_syntax, Exp}).

eval({'|', SubExps} = _Exp, Args) ->
    eval_or(SubExps, Args);

eval({'&', SubExps} = _Exp, Args) ->
    eval_and(SubExps, Args);

eval({'!', SubExp} = _Exp, Args) when is_tuple(SubExp) ->
    not eval(SubExp, Args);

eval({'>', Name, Val}, Args) ->
    {value, {_, ArgVal}} = keysearch(Name, 1, Args),
    ArgVal > to_integer(Val);

eval({'<', Name, Val}, Args) ->
    {value, {_, ArgVal}} = keysearch(Name, 1, Args),
    ArgVal < to_integer(Val);

eval({'!=', Name, Val}, Args) ->
    {value, {_, ArgVal}} = keysearch(Name, 1, Args),
    not (ArgVal == Val);

eval({'=', Name, Val}, Args) ->
     case keysearch(Name, 1, Args) of
     {value, {_, ArgVal}} -> ArgVal == Val;
     false -> false
     end;

eval({'>=', Name, Val}, Args) ->
    {value, {_, ArgVal}} = keysearch(Name, 1, Args),
    ArgVal >= to_integer(Val);

eval({'<=', Name, Val}, Args) ->
    {value, {_, ArgVal}} = keysearch(Name, 1, Args),
    ArgVal =< to_integer(Val).

eval_or([], _Args) ->
    false;
eval_or([SubExp|T], Args) ->
    case eval(SubExp, Args) of
    false -> eval_or(T, Args);
    true -> true
    end.

eval_and([], _Args) ->
    true;
eval_and([SubExp|T], Args) ->
    case eval(SubExp, Args) of
    true -> eval_and(T, Args);
    false -> false 
    end.

is_digital([]) ->
    false;

is_digital(L) ->
    is_digital2(L).

is_digital2([]) ->
    true;

is_digital2([H|T])  ->
    if
    (H >= $0) and (H =< $9) -> 
        is_digital2(T);
    true ->
        false
    end.

