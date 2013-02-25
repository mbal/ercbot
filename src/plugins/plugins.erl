%%%-------------------------------------------------------------------
%%% @author  mbal
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 24 Feb 2013 by  mbal
%%%-------------------------------------------------------------------
-module(plugins).

-behaviour(gen_server).

-export([init/1, handle_event/2, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([name/0]).

-define(SERVER, ?MODULE). 

name() ->
    none.

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

init([]) ->
    {ok, {}}.

handle_cast({cmd, Channel, Nick, "plugin", Args}, State) ->
    case irc_api:is_admin(Nick) of
        true ->
            handle_command(Channel, Args); 
        false ->
            irc_api:send_priv_msg(Channel, 
                                  "Only admins can use this command")
    end,
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Request, _, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

handle_event(_, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_command(Channel, ["reload"]) ->
    irc_api:reload_plugins(),
    irc_api:send_priv_msg(Channel, "Reloaded all plugins");
handle_command(Channel, ["load", Name]) ->
    case irc_api:load_plugin(Name) of
        ok ->
            irc_api:send_priv_msg(Channel, "reloaded");
        error ->
            irc_api:send_priv_msg(Channel, "Couldn't restart plugin")
    end;
handle_command(Channel, ["remove", Name]) ->
    case irc_api:remove_plugin(Name) of
        ok ->
            irc_api:send_priv_msg(Channel, "Correctly removed");
        error ->
            irc_api:send_priv_msg(Channel, "Could not remove plugin")
    end;
handle_command(Channel, _) ->
    irc_api:send_priv_msg(Channel, "Unknown command").

