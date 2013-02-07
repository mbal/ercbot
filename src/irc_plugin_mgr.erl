-module(irc_plugin_mgr).
-behaviour(gen_server).
-export([start_link/2]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

-record(state, {supervisor, bot, plug_event}).

start_link(Bot, Supervisor) ->
    utils:debug("~w starting...", [?MODULE]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Bot, Supervisor], []).

%gen_server's api.
init([Bot, Supervisor]) ->
    {ok, Plugins} = gen_event:start_link({local, evt_mgr}),
    gen_event:add_sup_handler(Plugins, uptime, [Bot]),
    gen_event:add_sup_handler(Plugins, tell_time, [Bot]),
    gen_event:add_sup_handler(Plugins, help_text, [Bot]),
    gen_event:add_sup_handler(Plugins, admin, [Bot]),
    {ok, #state{bot=Bot, supervisor=Supervisor, plug_event=Plugins}}.

handle_call(_Request, _From, State) ->
    {ok, State}.

handle_cast({cmd, Nick, Command, Args}, State) ->
    utils:debug("received command ~w", [now()]),
    gen_event:notify(State#state.plug_event, {cmd, Nick, Command, Args}),
    %gen_tcp:send(State#state.socket, Data ++ ?CRNL),
    {noreply, State};

%well, if other possible messages come to mind, we'll add them later
handle_cast({new_bot, Pid}, State) ->
    {noreply, State#state{bot=Pid}}.

handle_info({gen_event_EXIT, Handler, Reason}, State) ->
    utils:debug("a plugin crashed: ~w", [Handler]),
    gen_event:add_sup_handler(State#state.plug_event, Handler, [State#state.bot]),
    Msg = lists:flatten(io_lib:format("Plugin ~w crashed. Restarting...", [Handler])),
    irc_bot_api:send_priv_msg(State#state.bot, Msg),
    {noreply, State};
handle_info(_Req, State) ->
    io:format("~w", [_Req]),
    {noreply, State}.

terminate(_Reason, State) ->
    utils:debug("~w terminating", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    utils:debug("Code change for ~w", [?MODULE]),
    {ok, State}.
