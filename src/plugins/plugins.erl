%%%-------------------------------------------------------------------
%%% @author  mbal
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 24 Feb 2013 by  mbal
%%%-------------------------------------------------------------------
-module(plugins).

-behaviour(gen_event).

-export([init/1, handle_event/2, handle_call/2, 
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

handle_event({cmd, Channel, Nick, "plugin", Args}, State) ->
    case irc_api:is_admin(Nick) of
        true ->
            handle_command(Channel, Args); 
        false ->
            irc_api:send_priv_msg(Channel, "Only admins can use this command")
    end,
    {ok, State};

handle_event(_Msg, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
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
    irc_api:load_plugin(Name);
handle_command(Channel, ["remove", Name]) ->
    irc_api:remove_plugin(Name);
handle_command(Channel, _) ->
    irc_api:send_priv_msg(Channel, "Unknown command").

