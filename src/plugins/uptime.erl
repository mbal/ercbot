-module(uptime).

-behaviour(gen_event).
-record(state, {bot}).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).
-export([name/0, short_description/0]).

name() -> "uptime".
short_description() -> "bunch of uninteresting facts about uptime and memory".

init([Bot, _]) ->
    {ok, #state{bot=Bot}}.

uptime() ->
    {UpTime, _} = erlang:statistics(wall_clock),
    {D, {H, M, S}} = calendar:seconds_to_daystime(UpTime div 1000),
    lists:flatten(io_lib:format("~p days, ~p hours, ~p minutes and ~p seconds", [D,H,M,S])).

memory() ->
    M = proplists:get_value(total, erlang:memory()),
    lists:flatten(io_lib:format("memory: ~p kb", [M / 1000])).

handle_event({cmd, _, "uptime", _Args}, State) ->
    bot_fsm_api:send_priv_msg(State#state.bot, uptime()),
    bot_fsm_api:send_priv_msg(State#state.bot, memory()),
    {ok, State};
handle_event(_Req, State) ->
    {ok, State}.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
