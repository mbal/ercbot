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

-record(state, {bot, loaded_plugins, event_handler}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Bot) ->
    utils:debug("~w starting...", [?MODULE]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Bot], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Bot]) ->
    Plugins = conf_server:lookup(plugins),
    {ok, EvtMgr} = gen_event:start_link({local, evt_mgr}),
    LoadedPlugins = load_plugins(Plugins, EvtMgr),
    {ok, #state{bot=Bot, loaded_plugins=LoadedPlugins, event_handler=EvtMgr}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({cmd, Channel, _, "help", []}, State) ->
    CmdString = get_cmd_string(),
    irc_api:send_priv_msg(Channel, "Available plugins:"),

    AvPlugs = [X:name() || X <- State#state.loaded_plugins, X:name() /= none], 

    irc_api:send_priv_msg(Channel, string:join(AvPlugs, ", ")),
    irc_api:send_priv_msg(Channel, ["To get help on a specific plugin, use ",
                                    CmdString, "help <plugin>"]),
    {noreply, State};

handle_cast({cmd, Channel, _, "help", [Name]}, State) ->
    %% check if there's a plugin with PLUGIN:name() == Name,
    %% if so, call PLUGIN:help(), otherwise say "Plugin not found".
    List = lists:zip(State#state.loaded_plugins, 
                     get_names(State#state.loaded_plugins)),

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

handle_cast(reload, State) ->
    lists:foreach(fun(X) -> gen_event:delete_handler(
                              State#state.event_handler, X, shutdown) end,
                  State#state.loaded_plugins),

    %% let's get the new list of plugins.
    Plugins = conf_server:lookup(plugins),
    LoadedPlugins = load_plugins(Plugins, State#state.event_handler),
    {noreply, State#state{loaded_plugins=LoadedPlugins}};

handle_cast({reload, PluginName}, State) ->
    %% this message should do the opposite of {remove, PluginName}
    Plugins = conf_server:lookup(plugins),
    NotLoaded = Plugins -- State#state.loaded_plugins,
    List = lists:zip(NotLoaded, get_names(NotLoaded)),
    case lists:keyfind(PluginName, 2, List) of
        false ->
            Plugin2 = Plugins;
        {Module, _} ->
            %% okay, load plugin
            gen_event:add_sup_handler(State#state.event_handler, Module, []),
            Plugin2 = [Module | Plugins]
    end,
    {noreply, State#state{loaded_plugins=Plugin2}};

handle_cast({remove, PluginName}, State) ->
    List = lists:zip(State#state.loaded_plugins,
                     get_names(State#state.loaded_plugins)),
    case lists:keyfind(PluginName, 2, List) of
        false ->
            Plugins = State#state.loaded_plugins;
        {Module, _} ->
            gen_event:delete_handler(State#state.event_handler,
                                     Module,
                                     shutdown),
            Plugins = lists:delete(Module, State#state.loaded_plugins)
    end,
    {noreply, State#state{loaded_plugins=Plugins}};

%%% all messages are dispatched to the plugins, even priv_msg or the
%%% control-s not already handled by the bot.
handle_cast(Message, State) ->
    gen_event:notify(State#state.event_handler, Message),
    {noreply, State}.

handle_info({new_bot, Pid}, State) ->
    %%this plugin receives the `new_bot` info when the underlying bot
    %%crashes. This module must receive the new pid in order to send answers
    {noreply, State#state{bot=Pid}};

handle_info({gen_event_EXIT, _Handler, normal}, State) ->
    {noreply, State};
handle_info({gen_event_EXIT, Handler, Reason}, State) ->
    utils:debug("Plugin ~p crashed. ", [atom_to_list(Handler)]),
    case gen_event:add_sup_handler(State#state.event_handler, Handler, []) of
        ok ->
            utils:debug("Successfully restarted");
        {'EXIT', Reason} ->
            utils:debug("Couldn't restart (reason ~p)", [Reason])
    end,
    {noreply, State};

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
load_plugins(PluginList, EvtMgr) ->
    load_plugins2(PluginList, EvtMgr, []).

load_plugins2([], _, Acc) ->
    Acc;
load_plugins2([Plugin|Rest], EvtMgr, Acc) ->
    case gen_event:add_sup_handler(EvtMgr, Plugin, []) of
        ok ->
            load_plugins2(Rest, EvtMgr, [Plugin|Acc]);
        {'EXIT', Reason} ->
            utils:debug("Problem loading plugin: ~p, reason: ~p", 
                        [Plugin, Reason]),
            load_plugins2(Rest, EvtMgr, Acc)
    end.
