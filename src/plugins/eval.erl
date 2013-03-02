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
-export([lex/1, eval/3]).

-define(BADARITH, "Error! Division by zero?").
-define(THROWERROR, "Error! Number not in range").
-define(TOOBIG, "Error! The stack is too big!").
-define(GENERROR, "Error! Probably you added some useless operator").

-record(state, {prev_stack}).

name() ->
    "eval".
help() ->
    "Mini RPN calculator. Available operators: +, -, *, /, ^, ! "
        "and @ (previous stack content)".

init([]) ->
    {ok, #state{}}.

handle_event({cmd, Channel, _Nick, "eval", ExprList}, State) ->
    OldStack = State#state.prev_stack,
    try 
        Expression = lex(ExprList),
        NewStack = eval(Expression, [], OldStack),
        irc_api:send_priv_msg(Channel, 
                              string:join(
                                lists:map(
                                  fun(X) -> io_lib:format("~p", [X]) end, 
                                  NewStack), ", ")),
    {ok, State#state{prev_stack=NewStack}}
    catch
        error:badarith -> irc_api:send_priv_msg(Channel, ?BADARITH), {ok, State};
        throw:error -> irc_api:send_priv_msg(Channel, ?THROWERROR), {ok, State};
        throw:too_big -> irc_api:send_priv_msg(Channel, ?TOOBIG), {ok, State};
        _Err:_Reason -> irc_api:send_priv_msg(Channel, ?GENERROR), {ok, State}
    end;
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

-define(SUPPORTED_OP, ["+", "*", "-", "/", "^", "@", "!"]).

lex(Expression) ->
    lex1(Expression, []).

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
    eval(ExprList, [X-Y | Stack], OldStack);
eval(["*" | ExprList], [X, Y | Stack], OldStack) ->
    eval(ExprList, [X*Y | Stack], OldStack);
eval(["/" | ExprList], [X, Y | Stack], OldStack) ->
    eval(ExprList, [X/Y | Stack], OldStack);
eval(["^" | ExprList], [X, Y | Stack], OldStack) ->
    eval(ExprList, [pow(X, Y) | Stack], OldStack);

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
