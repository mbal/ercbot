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
    lists:foreach(fun(X) -> gen_event:add_sup_handler(EvtMgr, X, []) end,
                  Plugins),
    {ok, #state{bot=Bot, loaded_plugins=Plugins, event_handler=EvtMgr}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%this server handle only casts.
handle_cast({cmd, _, "help", _Args}, State) ->
    lists:foreach(fun(Plug) ->
                          gen_fsm:send_all_state_event(State#state.bot,
                                                       {reply_priv, Plug:name() ++ ": " ++ Plug:short_description()}) end,
                  State#state.loaded_plugins),
    {noreply, State};
handle_cast({cmd, _, _, _} = Message, State) ->
    %%we just dispatch to the plugins
    gen_event:notify(State#state.event_handler, Message),
    {noreply, State};
handle_cast(reload, State) ->
    lists:foreach(fun(X) -> gen_event:delete_handler(State#state.event_handler, X, shutdown) end,
                  State#state.loaded_plugins),
    %%let's get the new list of plugins.
    Plugins = conf_server:lookup(plugins),
    lists:foreach(fun(X) -> gen_event:add_sup_handler(State#state.event_handler, X, []) end,
                  Plugins),
    {noreply, State#state{loaded_plugins=Plugins}};
handle_cast(_, State) ->
    {noreply, State}.

handle_info({new_bot, Pid}, State) ->
    %%this plugin receives the `new_bot` info when the underlying bot
    %%crashes. This module must receive the new pid in order to send answers
    {noreply, State#state{bot=Pid}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
