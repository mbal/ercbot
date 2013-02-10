-module(admin).

-behaviour(gen_event).
-record(state, {bot, admins=settings:admin()}).

-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).
-export([name/0, short_description/0]).

name() -> "admin".
short_description() -> "perform simple administrative tasks".

init([Bot]) ->
    case ets:lookup(state_storage, ?MODULE) of
        [{?MODULE, StateData}] ->
            {ok, StateData};
        [] ->
            {ok, #state{bot=Bot}}
    end.

handle_event({cmd, Nick, "admin", Args}, State) ->
    case lists:member(Nick, State#state.admins) of
        true ->
            Res = handle_command(State, Args);
        false ->
            Res = State,
            bot_fsm_api:send_priv_msg(State#state.bot, "You're not on my list, sorry")
    end,
    {ok, Res};
handle_event(_Req, State) ->
    {ok, State}.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, State) -> 
    ets:insert(state_storage, {admin, State}),
    ok.

get_admins(State) -> string:join(State#state.admins, ", ").

handle_command(State, Args) when length(Args) =< 2 ->
    case Args of
        ["add", User] -> 
            State#state{admins=[User|State#state.admins]};
        ["rem", User] ->
            State#state{admins=lists:delete(User, State#state.admins)};
        ["list"] ->
            bot_fsm_api:send_priv_msg(State#state.bot, get_admins(State)), State;
        ["cnick", NewNick] ->
            bot_fsm_api:change_nick(State#state.bot, NewNick), State;
        ["restart"] ->
            bot_fsm_api:send_priv_msg(State#state.bot, "Recompiling "
                "and restarting"),
            %bot_fsm_api:restart_from_config(State#state.bot),
            bot_fsm_api:send_priv_msg(State#state.bot, "ok"), State;
        ["crash"] ->
            _ = 1/0,
            State;
        _ ->
            bot_fsm_api:send_priv_msg(State#state.bot, "Unrecognized or incomplete option"), State
    end;
handle_command(State, _Args) ->
    bot_fsm_api:send_priv_msg(State#state.bot, "Too many options").
