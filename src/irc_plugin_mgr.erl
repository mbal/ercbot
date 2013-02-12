-module(irc_plugin_mgr).
-behaviour(gen_server).
-export([start_link/2]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).
-export([reload_plugins/1]).

-record(state, {supervisor, bot, plug_event}).

start_link(Bot, Supervisor) ->
    utils:debug("~w starting...", [?MODULE]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Bot, Supervisor], []).

%%gen_server's api.
init([Bot, Supervisor]) ->
    {ok, Plugins} = gen_event:start_link({local, evt_mgr}),
    lists:foreach(fun(X) -> gen_event:add_sup_handler(Plugins, X, [Bot, self()]) end,
		  settings:plugins()),
    {ok, #state{bot=Bot, supervisor=Supervisor, plug_event=Plugins}}.

handle_call(_Request, _From, State) ->
    {ok, State}.

%%we handle the "help" command here to be able to get the list of all
%%the loaded plugins, so we can simply call plugin:name() - plugin:description()
handle_cast({cmd, _Nick, "help", _Args}, State) ->
    bot_fsm_api:send_priv_msg(State#state.bot, 
			      [settings:bname(), "(v", settings:version(), ") ",
			       settings:whois()]),
    lists:foreach(fun(X) -> bot_fsm_api:send_priv_msg(State#state.bot,
						      [X:name(), ": ", X:short_description()])
		  end, settings:plugins()),
    {noreply, State};

handle_cast({cmd, Nick, Command, Args}, State) ->
    utils:debug("Received command @ ~s", [tell_time:get_time()]),
    gen_event:notify(State#state.plug_event, {cmd, Nick, Command, Args}),
    {noreply, State};

handle_cast(terminate, State) ->
    lists:foreach(fun(X) -> gen_event:delete_handler(State#state.plug_event, X, shutdown) end, settings:plugins()),
    {stop, State};
%%well, if other possible messages come to mind, we'll add them later
handle_cast(reload, State) ->
    lists:foreach(fun(X) -> gen_event:delete_handler(State#state.plug_event, X, shutdown) end, settings:plugins()),
    lists:foreach(fun(X) -> gen_event:add_sup_handler(State#state.plug_event, X, [State#state.bot, self()]) end, settings:plugins()),
    bot_fsm_api:send_priv_msg(State#state.bot, "Reloaded all plugins!"),
    {noreply, State};
handle_cast({new_bot, Pid}, State) ->
    {noreply, State#state{bot=Pid}}.

handle_info({gen_event_EXIT, Handler, normal}, State) ->
    {noreply, State};
handle_info({gen_event_EXIT, Handler, _Reason}, State) ->
    gen_event:add_sup_handler(State#state.plug_event, Handler, [State#state.bot, self()]),
    Msg = lists:flatten(io_lib:format("Plugin ~w crashed. Restarting...", [Handler])),
    bot_fsm_api:send_priv_msg(State#state.bot, Msg),
    {noreply, State};
handle_info(_Req, State) ->
    io:format("~w", [_Req]),
    {noreply, State}.

terminate(_Reason, _State) ->
    utils:debug("~w terminating", [?MODULE]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    utils:debug("Code change for ~w", [?MODULE]),
    {ok, State}.


reload_plugins(Pid) ->
    gen_server:cast(Pid, reload).
