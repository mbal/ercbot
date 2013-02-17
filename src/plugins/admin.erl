-module(admin).

-behaviour(gen_event).
-record(state, {parent, admins}).

-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).
-export([name/0, short_description/0]).

name() -> "admin".
short_description() -> "perform simple administrative tasks".

init([]) ->
    case ets:lookup(state_storage, ?MODULE) of
        [{?MODULE, StateData}] ->
            {ok, StateData};
        [] ->
            Admins = conf_server:lookup(admin),
            {ok, #state{admins=Admins}}
    end.

handle_event({cmd, Nick, "admin", Args}, State) ->
    case lists:member(Nick, State#state.admins) of
        true ->
            Res = handle_command(State, Args);
        false ->
            Res = State,
            plugin_api:send_priv_msg(State#state.parent, "You're not on my list, sorry")
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
            NAdminList = [User|State#state.admins],
            conf_server:update(admin, NAdminList),
            State#state{admins=NAdminList};
        ["rem", User] ->
            NAdminList = lists:delete(User, State#state.admins),
            conf_server:update(admins, NAdminList),
            State#state{admins=NAdminList};
        ["list"] ->
            plugin_api:send_priv_msg(get_admins(State)), 
            State;
        ["cnick", NewNick] ->
            plugin_api:change_nick(NewNick),
            State;
        ["restart"] ->
            plugin_api:send_priv_msg("Restarting"),
            plugin_api:restart_bot(), 
            State;
        ["shutdown"] ->
            plugin_api:send_priv_msg("Goodbye, suckers!"),
            plugin_api:shutdown_bot(), 
            State;
        ["reload"] ->
            plugin_api:reload_plugins(),
            State;
        ["crash"] ->
            _ = 1/0,
            State;
        _ ->
            plugin_api:send_priv_msg("Unrecognized or incomplete option"),
            State
    end;
handle_command(State, _Args) ->
    plugin_mgr_api:send_priv_msg("Too many options"),
    State.
