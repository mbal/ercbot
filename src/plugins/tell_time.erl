-module(tell_time).

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, 
         handle_info/2, code_change/3]).
-export([name/0, help/0, get_time/0]).

name() -> "time".
help() -> "exactly as it sounds.".

init([]) ->
    {ok, {}}.

handle_event({cmd, Channel, _, "time", _Args}, State) ->
    irc_api:send_priv_msg(Channel, int_to_str(unix_time()) ++
                              " or, if you don't get epochs: " ++ get_time()),
    {ok, State};
handle_event(_Req, State) ->
    {ok, State}.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.

int_to_str(A) -> io_lib:format("~p", [A]).

unix_time() ->
    {M, S, _} = now(),
    M * 1000000 + S.

get_time(HMS) ->
    {H, M, S} = HMS,
    case M < 10 of
        true -> Ms = "0" ++ int_to_str(M);
        false -> Ms = int_to_str(M)
    end,
    case S < 10 of
        true -> Ss = "0" ++ int_to_str(S);
        false -> Ss = int_to_str(S)
    end,
    int_to_str(H) ++ ":" ++ Ms ++ ":" ++ Ss.

get_time() -> 
    {_, HMS} = calendar:local_time(),
    get_time(HMS).

