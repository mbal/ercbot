%%%-------------------------------------------------------------------
%%% @author  mbal
%%% @copyright (C) 2013, 
%%% @doc
%%% basic support for CTCP messages. Currently only PING and VERSION
%%% are implemented, though.
%%% @end
%%% Created : 21 Feb 2013 by  mbal
%%%-------------------------------------------------------------------
-module(ctcp).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).
-export([name/0]).

name() -> none.

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

init([]) ->
    {ok, {}}.

handle_event({ctcp, Nick, "PING", Data}, State) ->
    plugin_api:send_msg(["NOTICE ", Nick, " :", 1, "PING ", Data, 1]),
    {ok, State};
handle_event({ctcp, Nick, "VERSION", []}, State) ->
    plugin_api:send_msg(["NOTICE ", Nick, " :", 1, "VERSION ", 
                         conf_server:lookup(appname), " ",
                         conf_server:lookup(version), 1]),
    {ok, State};
handle_event(_Evt, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
