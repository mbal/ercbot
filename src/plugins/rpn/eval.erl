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

-include("macro.hrl").
%% API
-export([name/0, help/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-record(state, {prev_stack, regex}).

name() ->
    "eval".
help() ->
    "Mini calculator. Available operators: +, -, *, /, ^, ! "
        "and @ (previous stack content, only in RPN mode)"
        "Usage: !bot eval [rpn] <expression>".

init([]) ->
    {ok, Regex} = re:compile(?TOK_REGEX),
    {ok, #state{regex=Regex}}.

handle_event({cmd, Channel, _Nick, "eval", ["rpn" | ExprList]}, State) ->
    OldStack = State#state.prev_stack,
    Rpn = rpn:lex(ExprList),
    Ans = do_evaluation(Channel, Rpn, OldStack),
    {ok, State#state{prev_stack=Ans}};

handle_event({cmd, Channel, _Nick, "eval", ExprList}, State) ->
    OldStack = State#state.prev_stack,
    Expression = syard:tokenize(string:join(ExprList, " "), State#state.regex),
    Rpn = syard:transform(rpn:lex(Expression)),
    irc_api:send_priv_msg(Channel, ["Equivalent RPN: ", 
                                    io_lib:format("~p", [Rpn])]),
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

do_evaluation(Channel, Expression, OldStack) ->
    case rpn:evaluate(Expression, OldStack) of
        {ok, Ans} ->
            irc_api:send_priv_msg(
              Channel, string:join(lists:map(
                                     fun(X) -> io_lib:format("~p", [X]) end,
                                     Ans), ", "));
        Error ->
            irc_api:send_priv_msg(Channel, Error),
            Ans = OldStack
    end,
    Ans.

