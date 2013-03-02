%%%-------------------------------------------------------------------
%%% @author  mbal
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created :  1 Mar 2013 by  mbal
%%%-------------------------------------------------------------------
-module(eval).

-behaviour(gen_event).

%% API
-export([name/0, help/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-define(BADARITH, "Error! Division by zero?").
-define(THROWERROR, "Error! Number not in range").
-define(TOOBIG, "Error! The stack is too big!").
-define(GENERROR, "Error! Probably you added some useless operator").
-define(PARERROR, "Error! Unbalanced parenthesis").

-define(SUPPORTED_OP, ["+", "*", "-", "/", "^", "@", "!"]).

-record(state, {prev_stack}).

name() ->
    "eval".
help() ->
    "Mini calculator. Available operators: +, -, *, /, ^, ! "
        "and @ (previous stack content, only in RPN mode)"
        "Usage: !bot eval [rpn] <expression>".

init([]) ->
    {ok, #state{}}.

handle_event({cmd, Channel, _Nick, "eval", ["rpn" | ExprList]}, State) ->
    OldStack = State#state.prev_stack,
    Rpn = lex(ExprList),
    Ans = do_evaluation(Channel, Rpn, OldStack),
    {ok, State#state{prev_stack=Ans}};

handle_event({cmd, Channel, _Nick, "eval", ExprList}, State) ->
    OldStack = State#state.prev_stack,
    Expression = tokenize(string:join(ExprList, " ")),
    Rpn = transform(lex(Expression)),
    irc_api:send_priv_msg(Channel, ["Equivalent RPN: ", io_lib:format("~p", [Rpn])]),
    Ans = do_evaluation(Channel, Rpn, OldStack),
    {ok, State#state{prev_stack=Ans}};

handle_event(_Evt, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

tokenize(String) ->
    {match, List} = re:run(String, "([0-9]+(?:\\.[0-9]+)?)|(\\+|\\-|/|\\*|\\!|\\@)|([\\(\\)])",
                           [global, {capture, all_but_first, list}]),
    io:format(">~p~n", [List]),
    List2 = lists:foldl(fun(X, Y) ->
                                Y ++ X end, [], List),
    lists:filter(fun(X) -> X /= [] end, List2).

do_evaluation(Channel, Expression, OldStack) ->
    case evaluate(Expression, OldStack) of
        {ok, Ans} ->
            irc_api:send_priv_msg(Channel, 
                                  string:join(
                                    lists:map(fun(X) -> io_lib:format("~p", [X]) end,
                                              Ans), ", "));
        Error ->
            irc_api:send_priv_msg(Channel, Error),
            Ans = OldStack
    end,
    Ans.

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
    N * factorial(N - 1);
factorial(_) ->
    throw(error).

priority("+") -> 1;
priority("-") -> 1;
priority("^") -> 2;
priority("*") -> 3;
priority("/") -> 3;
priority("!") -> 4;
priority("@") -> 5.

assoc("!") -> right;
assoc("^") -> right;
assoc(_)   -> left.

is_operator(Op) ->
    lists:member(Op, ?SUPPORTED_OP).

transform(Expression) ->
    transform1(Expression, [], []).

transform1(["(" | Rest], OutStack, OpStack) ->
    transform1(Rest, OutStack, ["(" | OpStack]);
transform1([")" | Rest], OutStack, OpStack) ->
    {OutStack2, OpStack2} = transform_par(OutStack, OpStack),
    transform1(Rest, OutStack2, OpStack2);
transform1([H|Rest], OutStack, OpStack) ->
    case is_operator(H) of
        true ->
            {Stack, OpStack2} = transform_op(H, OutStack, OpStack),
            transform1(Rest, Stack, OpStack2);
        false ->
            transform1(Rest, [H | OutStack], OpStack)
    end;
transform1([], OutStack, [Op|OpStack]) ->
    transform1([], [Op|OutStack], OpStack);
transform1([], OutStack, []) ->
    lists:reverse(OutStack).

transform_par(_OutStack, []) ->
    throw(par_error);
transform_par(OutStack, ["(" | OpStack]) ->
    {OutStack, OpStack};
transform_par(OutStack, [Op | OpStack]) ->
    transform_par([Op | OutStack], OpStack).

transform_op(Op, Stack, []) ->
    {Stack, [Op]};
transform_op(Op1, Stack, ["("|_]=CStack) ->
    {Stack, [Op1|CStack]};
transform_op(Op1, Stack, [Op2|OpStack]=CStack) ->
    case ((assoc(Op1) == left) and (priority(Op1) =< priority(Op2)) or
                                                                      (priority(Op1) < priority(Op2))) of
        true ->
            transform_op(Op1, [Op2 | Stack], OpStack);
        false ->
            {Stack, [Op1 | CStack]}
    end.
