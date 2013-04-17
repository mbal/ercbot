-module(rpn).
-export([lex/1, evaluate/2]).
-include("macro.hrl").

-define(BADARITH, "Error! Division by zero?").
-define(THROWERROR, "Error! Number not in range").
-define(TOOBIG, "Error! The stack is too big!").
-define(GENERROR, "Error! Probably you added some useless operator").
-define(PARERROR, "Error! Unbalanced parenthesis").

evaluate(Rpn, OldStack) ->
    try
        Res = eval(Rpn, [], OldStack),
        {ok, Res}
    catch
        error:badarith -> ?BADARITH;
        throw:error -> ?THROWERROR;
        throw:too_big -> ?TOOBIG;
        throw:par_error -> ?PARERROR;
        _Err:_Reason -> ?GENERROR
    end.

lex(Expression) ->
    lex1(Expression, []).
lex1(["("|Expr], Res) ->
    lex1(Expr, ["(" | Res]);
lex1([")"|Expr], Res) ->
    lex1(Expr, [")" | Res]);
lex1([H|Expression], Result) when length(Result) < 1000 ->
    case lists:member(H, ?SUPPORTED_OP) of
        true ->
            lex1(Expression, [H|Result]);
        false ->
            case string:to_integer(H) of
                {error, _} ->
                    throw(error);
                {Number, _} ->
                    lex1(Expression, [Number|Result])
            end
    end;
lex1([], Res) ->
    lists:reverse(Res);
lex1(_Expr, _Res) ->
    throw(too_big).

eval(_, Stack, _) when length(Stack) > 100 ->
    throw(too_big);
eval(["+" | ExprList], [X, Y | Stack], OldStack) ->
    eval(ExprList, [X+Y | Stack], OldStack);
eval(["-" | ExprList], [X, Y | Stack], OldStack) ->
    eval(ExprList, [Y-X | Stack], OldStack);
eval(["*" | ExprList], [X, Y | Stack], OldStack) ->
    eval(ExprList, [X*Y | Stack], OldStack);
eval(["/" | ExprList], [X, Y | Stack], OldStack) ->
    eval(ExprList, [Y/X | Stack], OldStack);
eval(["^" | ExprList], [X, Y | Stack], OldStack) ->
    eval(ExprList, [pow(Y, X) | Stack], OldStack);
eval(["@" | ExprList], Stack, OldStack) ->
    eval(ExprList, OldStack ++ Stack, OldStack);
eval(["!" | ExprList], [X | Stack], OldStack) ->
    eval(ExprList, [factorial(X) | Stack], OldStack);
eval([E | ExprList], Stack, OldStack) ->
    eval(ExprList, [E | Stack], OldStack);
eval([], Result, _OldStack) ->
    Result.

pow(A, B) when A < 100, B < 100 ->
    math:pow(A, B);
pow(_, _) ->
    throw(error).

factorial(N) when is_float(N) ->
    factorial(trunc(N));
factorial(0) ->
    1;
factorial(N) when (N > 0) andalso (N < 700) ->
    factorial_tail(N, 1);
factorial(_) ->
    throw(error).

factorial_tail(1, Acc) ->
    Acc;
factorial_tail(N, Acc) ->
    factorial_tail(N-1, N*Acc).

