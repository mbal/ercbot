-module(uptime).

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, 
         handle_info/2, code_change/3]).
-export([name/0, help/0]).

name() -> "uptime".
help() -> "couple of uninteresting facts about uptime and memory".

init([]) ->
    {ok, {}}.

uptime() ->
    {UpTime, _} = erlang:statistics(wall_clock),
    {D, {H, M, S}} = calendar:seconds_to_daystime(UpTime div 1000),
    lists:flatten(io_lib:format("~p days, ~p hours, ~p minutes and ~p seconds",
                                [D, H, M, S])).

memory() ->
    M = proplists:get_value(total, erlang:memory()),
    lists:flatten(io_lib:format("memory: ~p kb", [M / 1000])).

handle_event({cmd, Channel, _, "uptime", _Args}, State) ->
    irc_api:send_priv_msg(Channel, uptime()),
    irc_api:send_priv_msg(Channel, memory()),
    {ok, State};
handle_event(_Req, State) ->
    {ok, State}.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
