%%%-------------------------------------------------------------------
%%% @author  mbal
%%% @copyright (C) 2013, 
%%% @doc
%%%  Module to connect to the server and dispatch all the receveid 
%%% messages to the bot. 
%%% This process is started by the bot, not as a standalone.
%%% @end
%%% Created : 19 Feb 2013 by  mbal
%%%-------------------------------------------------------------------
-module(irc_connector).

-behaviour(gen_server).

%% API
-export([start_link/1, send_priv_msg/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(CRNL, "\r\n").

-record(state, {supervisor, server, bot, socket}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Bot) ->
    utils:debug("~w starting...", [?MODULE]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Bot], []).

send_priv_msg(Msg) ->
    gen_server:call(?MODULE, {send, Msg}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Bot]) ->
    {Server, Port} = conf_server:lookup(server),
    case gen_tcp:connect(Server, Port, [{packet, line}]) of
        {ok, Socket} -> 
            gen_fsm:send_event(Bot, connected),
            {ok, #state{server=Server, bot=Bot, socket=Socket}};
        _ -> gen_fsm:send_event(Bot, connection_failed)
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%% two types of cast:
%%%    new_bot: informs the module that the bot has changed pid
%%%    send: receive data from another process to be sent via the connection
handle_cast({new_bot, Pid}, State) ->
    {noreply, State#state{bot=Pid}};
handle_cast({send, Data}, State) ->
    %% `Data` isn't CRNL terminated, so we simply add it.
    gen_tcp:send(State#state.socket, Data ++ ?CRNL),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({tcp, _Socket, Data}, State) ->
    Lines = string:tokens(Data, ?CRNL),
    send_fsm_message(State#state.bot, Lines),
    {noreply, State};
handle_info({tcp_closed, _Socket, _Data}, State) ->
    utils:debug("Connection closed"),
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

send_fsm_message(_, []) ->
    ok;
send_fsm_message(Dest, [Line|Rest]) ->
    gen_fsm:send_event(Dest, {recv, Line}),
    send_fsm_message(dest, Rest).

