-module(irc_connector).
-behaviour(gen_server).
-define(CRNL, "\r\n").
-export([start_link/3]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, init/1, terminate/2]).

-record(state, {supervisor, server, bot, socket, processor}).

start_link({_Server, _Port}=Where, Parent, Supervisor) ->
    utils:debug("~w starting...", [?MODULE]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Where, Parent, Supervisor], []).

%gen_server's api.
init([{Server, Port}, Parent, Processor]) ->
    case gen_tcp:connect(Server, Port, [{packet, line}]) of
        {ok, Socket} -> 
            gen_fsm:send_event(Parent, connected),
            {ok, #state{server=Server, bot=Parent, socket=Socket,
                    processor=Parent, supervisor=Processor}};
        _ -> gen_fsm:send_event(Parent, connection_failed)
    end.

%this bot doesn't handle call, everything should be a
%cast (that is, without return values)
handle_call(_Request, _From, State) ->
    {ok, State}.

handle_cast({send, Data}, State) ->
    gen_tcp:send(State#state.socket, Data ++ ?CRNL),
    {noreply, State};
%well, if other possible messages come to mind, we'll add them later
handle_cast({new_bot, Pid}, State) ->
    {noreply, State#state{bot=Pid, processor=Pid}};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({tcp, _Socket, Data}, State) ->
    Lines = string:tokens(Data, ?CRNL),
    send_message(State#state.processor, Lines),
    {noreply, State};
handle_info({tcp_closed, _Socket}, State) ->
    utils:debug("Connection closed."),
    {noreply, State};
handle_info(_Req, State) ->
    utils:debug("~w ~w", [_Req, State]),
    {noreply, State}.

terminate(_Reason, State) ->
    utils:debug("irc_connector terminating"),
    gen_fsm:send_event(State#state.processor, terminating),
    %error reporting is automagically issued, if the reason
    %is not normal or shutdown.
    ok.

code_change(_OldVsn, State, _Extra) ->
    utils:debug("Code change for IRCCONN"),
    {ok, State}.

%private functions

send_message(_, []) ->
    ok;
send_message(Dest, [Line|Rest]) ->
    gen_fsm:send_event(Dest, {recv, Line}),
    send_message(dest, Rest).
