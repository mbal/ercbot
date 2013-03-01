%%%-------------------------------------------------------------------
%%% @author  <Utente@UTENTE-UTENTE>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created :  1 Mar 2013 by  <Utente@UTENTE-UTENTE>
%%%-------------------------------------------------------------------
-module(eval).

-behaviour(gen_event).

%% API
-export([name/0, help/0]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([eval/2, pow/2,factorial/1]).

name() ->
    "eval".
help() ->
    "mini stack-based programming language".

-define(SERVER, ?MODULE). 

-record(state, {prev_result}).

init([]) ->
    {ok, #state{}}.

handle_event({cmd, Channel, _Nick, "eval", ExprList}, State) ->
    case eval(ExprList, []) of
        error -> irc_api:send_priv_msg(Channel, "Error, Error, Error, Error, Error");
        Stack -> irc_api:send_priv_msg(Channel, 
                                       string:join(
                                         lists:map(fun(X) -> io_lib:format("~p", [X]) end,
                                                   Stack), ", "))
    end,
    {ok, State};
handle_event(_Evt, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

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
    try
        Fact = factorial(X),
        eval(Rest, [Fact | Stack])
    catch
        _:_ -> error
    end;
eval(["^" | Rest], [X, Y | Stack]) ->
    eval(Rest, [pow(X, Y) | Stack]);
eval([C | Rest], Stack) ->
    case string:to_integer(C) of
        {error, _} ->
            error;
        {Number, _} ->
            eval(Rest, [Number | Stack]);
        _ ->
            error
    end;
eval([], Stack) ->
    Stack.

pow(A, B) ->
    pow(A, B, 1).
pow(A, 0, Res) ->
    Res;
pow(A, B, Res) when Res < 10000 ->
    pow(A, B-1, A * Res);
pow(_, _, _) ->
    error.


factorial(0) ->
    1;
factorial(N) when N > 0 andalso N < 1000 ->
    N * factorial(N - 1);
factorial(_) ->
    throw(error).
