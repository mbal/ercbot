%%%-------------------------------------------------------------------
%%% @author  Matteo Bana
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 13 Feb 2013 by  Matteo Bana
%%%-------------------------------------------------------------------
-module(plugin_mgr).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {supervisor, 
                evt_plugins, plugin_sup,
                event_manager, ser_plugins}).

-define(PLUGSUP, {plug_sup, 
                  {plugin_sup, start_link, []},
                  transient,
                  1000,
                  supervisor,
                  [plugin_supervisor]}).

-define(ALL_PLUGINS(STATE), STATE#state.evt_plugins ++ STATE#state.ser_plugins).

-define(CHILD_SPEC(Plugin), {Plugin,
                             {Plugin, start_link, []},
                             transient, 1000,
                             worker, [Plugin]}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Sup) ->
    utils:debug("~w starting...", [?MODULE]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Sup], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Sup]) ->
    {ok, EvtPid} = gen_event:start_link({local, event_manager}),
    EPlugins = conf_server:lookup(eplugins),
    LoadedEPlugins = load_eplugins(EPlugins, EvtPid),
    self() ! { start_splugin, Sup },
    {ok, #state{supervisor=Sup, event_manager=EvtPid,
                evt_plugins=LoadedEPlugins}}.

handle_cast(terminate, State) ->
    gen_event:stop(State#state.event_manager),
    {stop, shutdown, State};

handle_cast({cmd, Channel, _, "help", []}, State) ->
    CmdString = get_cmd_string(),
    irc_api:send_priv_msg(Channel, "Available plugins:"),
    
    AvPlugs = [X:name() || X <- ?ALL_PLUGINS(State), X:name() /= none], 

    irc_api:send_priv_msg(Channel, string:join(AvPlugs, ", ")),
    irc_api:send_priv_msg(Channel, ["To get help on a specific plugin, use ",
                                    CmdString, "help <plugin>"]),
    {noreply, State};

handle_cast({cmd, Channel, _, "help", [Name]}, State) ->
    %% check if there's a plugin with PLUGIN:name() == Name,
    %% if so, call PLUGIN:help(), otherwise say "Plugin not found".
    List = lists:zip(?ALL_PLUGINS(State), 
                     get_names(?ALL_PLUGINS(State))),

    CmdString = get_cmd_string(),

    case lists:keyfind(Name, 2, List) of
        false ->
            irc_api:send_priv_msg(Channel, "No such plugin!");
        {Module, _} ->
            try
                Help = Module:help(),
                FmtHelp = re:replace(Help, "%cmdstring% ", CmdString),
                irc_api:send_priv_msg(Channel, FmtHelp)
            catch
                error:undef ->
                    irc_api:send_priv_msg(Channel, "That plugin didn't "
                                          "provide a help text")
            end
    end,
    {noreply, State};

handle_cast(Message, State) ->
    lists:foreach(fun(Plugin) -> Plugin:cast(Message) end,
                  State#state.ser_plugins),
    gen_event:notify(State#state.event_manager, Message),
    {noreply, State}.

handle_call(reload, _From, State) ->
    EPlugins = State#state.evt_plugins,
    lists:foreach(fun(X) -> gen_event:delete_handler(
                              State#state.event_manager, X, shutdown) end,
                  EPlugins),
    ListPlugins = conf_server:lookup(eplugins),
    Plugins = load_eplugins(ListPlugins, State#state.event_manager),
    {reply, {ok, Plugins}, State#state{evt_plugins=Plugins}};

handle_call({reload, PluginName}, _From, State) ->
    Plugins = conf_server:lookup(eplugins),
    Loaded = State#state.evt_plugins,
    NotLoaded = Plugins -- Loaded,
    List = lists:zip(NotLoaded, get_names(NotLoaded)),
    case lists:keyfind(PluginName, 2, List) of
        false ->
            {reply, error, State};
        {Module, _} ->
            case gen_event:add_sup_handler(State#state.event_manager, 
                                           Module, []) of
                ok -> 
                    {reply, ok, State#state{evt_plugins=[Module|Loaded]}};
                {'EXIT', _Reason} ->
                    utils:debug("Couldn't restart ~p", [Module]),
                    {reply, error, State}
            end
    end;

handle_call({remove, PluginName}, _From, State) ->
    Stoppable = State#state.evt_plugins,
    List = lists:zip(Stoppable, get_names(Stoppable)),
    case lists:keyfind(PluginName, 2, List) of
        false ->
            {reply, error, State};
        {Module, _} ->
            gen_event:delete_handler(State#state.event_manager, 
                                     Module, shutdown),
            Plugins = lists:delete(Module, State#state.evt_plugins),
            {reply, ok, State#state{evt_plugins=Plugins}}
    end;

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_info({start_splugin, Sup}, State) ->
    Plugins = conf_server:lookup(splugins),
    PluginSup = case supervisor:start_child(Sup, ?PLUGSUP) of
                    {ok, Pid} -> Pid;
                    {error, {already_started, Pid}} -> Pid;
                    What -> io:format("~p", [What]), error
                end,
    SPlugins = load_plugins(Plugins, PluginSup),
    {noreply, State#state{plugin_sup=PluginSup, ser_plugins=SPlugins}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_names(Plugins) ->
    lists:map(fun(X) -> X:name() end, Plugins).

%%% helper function that returns the `cmdstring` with the correct 
%%% spacing, using the same rules in utils:parse_tokens
get_cmd_string() ->
    CmdString = conf_server:lookup(cmd_string),
    case length(CmdString) of
        1 -> CmdString;
        _ -> CmdString ++ " "
    end.

%%% returns the list of the plugins that have been loaded
load_plugins(PluginList, Supervisor) ->
    load_plugins2(PluginList, Supervisor, []).

load_plugins2([], _, Acc) ->
    Acc;
load_plugins2([Plugin|Rest], Supervisor, Acc) ->
    case supervisor:start_child(Supervisor, ?CHILD_SPEC(Plugin)) of
        {ok, _Pid} ->
            load_plugins2(Rest, Supervisor, [Plugin|Acc]);
        {error, Reason} ->
            utils:debug("Problem loading plugin: ~p, reason: ~p", 
                        [Plugin, Reason]),
            load_plugins2(Rest, Supervisor, Acc)
    end.

%% returns the list of the plugins that have been loaded
%% used to load plugins with the gen_event behaviour
load_eplugins(PlugList, EvtMgr) ->
    load_eplugins2(PlugList, EvtMgr, []).

load_eplugins2([], _, Acc) ->
    Acc;
load_eplugins2([Plugin|Rest], EvtMgr, Acc) ->
    case gen_event:add_sup_handler(EvtMgr, Plugin, []) of
        ok ->
            load_eplugins2(Rest, EvtMgr, [Plugin|Acc]);
        {'EXIT', Reason} ->
            utils:debug("Problem loading plugin ~p, reason ~p",
                        [Plugin, Reason]),
            load_eplugins2(Rest, EvtMgr, Acc)
    end.
