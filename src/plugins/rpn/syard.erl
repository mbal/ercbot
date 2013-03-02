%%% @author  mbal
%%% @copyright (C) 2013, 
%%% @doc
%%% Shunting-Yard algorithm implementation.
%%% @end
%%% Created :  2 Mar 2013 by  mbal

-module(syard).
-export([transform/1, tokenize/2, tokenize/1]).
-include("macro.hrl").

%%--------------------------------------------------------------------
%% @doc 
%% splits an arithmetical expression into a list of tokens. Very
%% rough, but it seems to work 
%% @end
%% --------------------------------------------------------------------
tokenize(String) ->
    tokenize(String, ?TOK_REGEX).

tokenize(String, Regex) ->
    {match, List} = re:run(String, Regex,
                           [global, {capture, all_but_first, list}]),
    List2 = lists:foldl(fun(X, Y) -> Y ++ X end, [], List),
    lists:filter(fun(X) -> X /= [] end, List2).

%%--------------------------------------------------------------------
%% @doc
%% transform is an implementation of Shunting-Yard.
%% @spec
%% transform(string()) -> [number() | string()].
%% @end
%%--------------------------------------------------------------------
-spec(transform(string()) -> [number() | string()]).

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
    case ((assoc(Op1) == left) and (priority(Op1) =< priority(Op2))) or
        (priority(Op1) < priority(Op2)) of
        true ->
            transform_op(Op1, [Op2 | Stack], OpStack);
        false ->
            {Stack, [Op1 | CStack]}
    end.

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
