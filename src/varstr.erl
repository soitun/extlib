%%%----------------------------------------------------------------------
%%% File    : chash.erl
%%% Author  : Ery Lee <ery.lee@gmail.com>
%%% Purpose : String contains variables
%%% Created : 20 Mar. 2011
%%% License : http://www.opengoss.com
%%%
%%% Copyright (C) 2011, www.opengoss.com 
%%%----------------------------------------------------------------------

%%%Example: "My name is ${name}"

-module(varstr).

-import(lists, [reverse/1]).

-export([scan/1, 
        eval/2]).

eval(VarStr, VarList) ->
    Tokens = scan(VarStr),
    Words = 
    lists:map(fun(Token) -> 
    case Token of
    "$" ++ VarName -> 
        {value, Val} = dataset:get_value(list_to_atom(VarName), VarList, ""),
        str(Val);
    _ -> 
        Token
    end
    end, Tokens),
    string:join(Words, "").

scan([]) ->
    [];
scan(Expr) ->
    scan_start(Expr, [], []).

scan_start([$$|S], Token, Tokens) ->
    var_start(S, Token, Tokens);
scan_start([C|S], Token, Tokens) ->
    scan_str(S, [C|Token], Tokens).

var_start([${|S], _Token, Tokens) ->
    scan_var1(S, [], Tokens);
var_start(S, _Token, Tokens) ->
    scan_var(S, [], Tokens).

scan_var([C|S], Token, Tokens) when ((C =< $9) and (C >= $0))
    or ((C =< $Z) and (C >= $A))
    or ((C =< $z) and (C >= $a))
    or ((C == $-) or (C == $_)) ->
    scan_var(S, [C|Token], Tokens);

scan_var(S, Token, Tokens) ->
    Var = [$$ | reverse(Token)],
    end_var(S, [], [Var|Tokens]).

scan_var1([$}|S], Token, Tokens) ->
    Var = [$$ | reverse(Token)],
    end_var(S, [], [Var|Tokens]);
scan_var1([C|S], Token, Tokens) ->
    scan_var1(S, [C|Token], Tokens).

end_var([], _, Tokens) ->
    reverse(Tokens);
end_var([$$|S], Token, Tokens) ->
    var_start(S, Token, Tokens);
end_var(S, Token, Tokens) ->
    scan_str(S, Token, Tokens).

scan_str([], Token, Tokens) ->
    Str = reverse(Token),
    reverse([Str|Tokens]);
scan_str([$$|S], Token, Tokens) ->
    Str = reverse(Token),
    var_start(S, [], [Str|Tokens]);
scan_str([C|S], Token, Tokens) ->
    scan_str(S, [C|Token], Tokens).

str(Val) when is_integer(Val) ->
	integer_to_list(Val);
str(Val) when is_float(Val) ->
	string:join(io_lib:format("~.2f", [Val]),"");
str(Val) when is_list(Val) ->
	Val;
str(Val) when is_binary(Val) ->
	binary_to_list(Val);
str(_Val) ->
	"".

