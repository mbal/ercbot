-module(help_text).

-behaviour(gen_event).
-record(state, {bot}).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

init([Bot]) ->
    {ok, #state{bot=Bot}}.

handle_event({cmd, _, "help", _Args}, State) ->
    Head = lists:flatten(io_lib:format("~s(v~s), an extensible Erlang bot.", [settings:bname(), settings:version()])),
    irc_bot_api:send_priv_msg(State#state.bot, Head),
    irc_bot_api:send_priv_msg(State#state.bot, "Available commands:"),
    irc_bot_api:send_priv_msg(State#state.bot, "uptime, time"),
    irc_bot_api:send_priv_msg(State#state.bot, "help"),
    %irc_bot_api:send_priv_msg(State#state.bot, "quote"),
    irc_bot_api:send_priv_msg(State#state.bot, "talk <length> [order]"),
    {ok, State};
handle_event(_Req, State) ->
    {ok, State}.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
