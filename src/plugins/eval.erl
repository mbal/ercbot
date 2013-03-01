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

-define(SERVER, ?MODULE). 
-define(BADARITH, "Division by zero?").
-define(THROWERROR, "Number not in range").
-define(GENERROR, "Probably you added some useless operator").

name() ->
    "eval".
help() ->
    "mini rpn calculator".

init([]) ->
    {ok, {}}.

handle_event({cmd, Channel, _Nick, "eval", ExprList}, State) ->
    try 
        Stack = eval(ExprList, []),
        irc_api:send_priv_msg(Channel, 
                              string:join(
                                lists:map(
                                  fun(X) -> io_lib:format("~p", [X]) end, 
                                  Stack), ", ")) 
    catch
        error:badarith -> irc_api:send_priv_msg(Channel, ?BADARITH);
        throw:error -> irc_api:send_priv_msg(Channel, ?THROWERROR);
        _:_ -> irc_api:send_priv_msg(Channel, ?GENERROR)
    end,
    {ok, State};
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

eval(["/" | Rest], [X, Y | Stack]) ->
    eval(Rest, [X/Y | Stack]);
eval(["-" | Rest], [X, Y | Stack]) ->
    eval(Rest, [X-Y | Stack]);
eval(["*" | Rest], [X, Y | Stack]) ->
    eval(Rest, [X*Y | Stack]);
eval(["+" | Rest], [X, Y | Stack]) ->
    eval(Rest, [X+Y | Stack]);
eval(["!" | Rest], [X | Stack]) ->
    eval(Rest, [factorial(X) | Stack]);
eval(["^" | Rest], [X, Y | Stack]) ->
    eval(Rest, [pow(X, Y) | Stack]);
eval([C | Rest], Stack) ->
    case string:to_integer(C) of
        {error, _} ->
            throw(error);
        {Number, _} ->
            eval(Rest, [Number | Stack]);
        _ ->
            throw(error)
    end;
eval([], Stack) ->
    Stack.

pow(A, B) ->
    pow(A, B, 1).

pow(_A, 0, Res) ->
    Res;
pow(A, B, Res) when abs(Res) < 10000 ->
    pow(A, B-1, A * Res);
pow(_, _, _) ->
    throw(error).

factorial(0) ->
    1;
factorial(N) when N > 0 andalso N < 1000 ->
    N * factorial(N - 1);
factorial(_) ->
    throw(error).
