-module(bot_fsm).
-behaviour(gen_fsm).

-record(state, {nick, channel, connection, supervisor, pluginmgr, server}).

-define(WHOIS, settings:whois()).

-define(CHILD_SPEC(NAME, M, F, A), 
    {NAME,
        {M, F, A},
        permanent,
        1000,
        worker,
        [M]}).

-define(CHILD_IRC_CONN(WHERE, SUP), {irc_connector, [WHERE, self(), SUP]}).
-define(CHILD_IRC_PLUGIN(SUP), {irc_plugin_mgr, [self(), SUP]}).

-export([start_link/4, init/1]).
-export([code_change/4, handle_event/3, handle_info/3]).
-export([handle_sync_event/4, terminate/3]).
-export([logged/2, idle/2, ready/2]).

start_link(Sup, Nick, Channel, Server) ->
    utils:debug("~w starting...", [?MODULE]),
    case ets:lookup(state_storage, irc_bot) of
        [{irc_bot, NextState, NextData}] ->
            gen_fsm:start_link({local, ?MODULE}, ?MODULE, [Sup, NextState, NextData, Nick, Channel], []);
        [] ->
            gen_fsm:start_link({local, ?MODULE}, ?MODULE, [Sup, idle, #state{server=Server}, Nick, Channel], []);
        _ -> ok 
    end.

init([Sup, NextState, NextData, Nick, Channel]) ->
    self() ! {start_connection, Sup},
    {ok, NextState, 
        NextData#state{supervisor=Sup, nick=Nick, channel=Channel}}.

idle(connected, State) ->
    send_msg(State, ["NICK ", State#state.nick]),
    send_msg(State, ["USER ", State#state.nick, " 8 * : ", ?WHOIS]),
    {next_state, logged, State};
idle(_, State) ->
    {next_state, idle, State}.

logged({recv, Data}, State) ->
    Res = utils:irc_parse(Data), %string:tokens(Data, ": ")),
    case Res of
        {control, join} ->
            send_msg(State, ["JOIN :", State#state.channel]),
            {next_state, ready, State};
        {control, ping, Data} ->
            utils:debug("Received PING, replying"),
            reply_ping(State, Data);
        {control, change_nick} ->
            {next_state, logged, State};
        _ -> {next_state, logged, State}
    end.

ready({recv, Msg}, State) ->
    Res = utils:irc_parse(Msg), %string:tokens(Msg, ": ")),
    case Res of
        %TODO: delete these two lines
        {cmd, _Nick, "crash", _Args} ->
            _ = 1/0;
        {cmd, Nick, Cmd, Args} -> 
            gen_server:cast(State#state.pluginmgr, {cmd, Nick, Cmd, Args});
        {control, ping, Data} ->
            utils:debug("Received PING, replying"),
            reply_ping(State, Data);
        _ -> ok
    end,
    {next_state, ready, State}.

code_change(_Old, _, _, _) -> 
    utils:debug("Code change event received"),
    ok.

%this collects all the responses from the plugins.
handle_event({reply_priv, Msg}, State, Data) -> 
    send_priv_msg(Data, Msg),
    {next_state, State, Data};
handle_event({reply_command, Msg}, State, Data) -> 
    send_msg(Data, Msg),
    {next_state, State, Data};
handle_event({change_nick, NNick}, State, Data) ->
    send_msg(Data, ["NICK ", NNick]),
    {next_state, State, Data#state{nick=NNick}};
%handle_event(restart, State, Data) ->
    %utils:recompile(all),
    
handle_event(Evt, State, Data) ->
    utils:debug("Unknown event ~w!", [Evt]),
    {next_state, State, Data}.

handle_info({start_connection, Sup}, StateName, State) ->
    %in this function the bot starts all the other child of the supervisor
    ConPid = start_process(Sup, irc_conn, start_link, ?CHILD_IRC_CONN(State#state.server, Sup)),
    PlgPid = start_process(Sup, irc_plug, start_link, ?CHILD_IRC_PLUGIN(Sup)),
    {next_state, StateName, State#state{pluginmgr=PlgPid, connection=ConPid}};

handle_info(_Msg, State, Data) -> 
    {next_state, State, Data}.

handle_sync_event(_Event, _From, StateName, Data) -> 
    {next_state, StateName, Data}.

terminate(_Reason, CurrentState, CData) ->
    utils:debug("Encountered error, saving state... ~w and ~w", [CurrentState, CData]),
    send_priv_msg(CData, "Bot encountered an error, restarting..."),
    ets:insert(state_storage, {irc_bot, CurrentState, CData}),
    ok.

start_process(Sup, Name, F, {M, A}) ->
    case supervisor:start_child(Sup, ?CHILD_SPEC(Name, M, F, A)) of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} ->
            gen_server:cast(Pid, {new_bot, self()}),
            Pid;
        _ -> none
    end.

send_priv_msg(State, Msg) ->
    gen_server:cast(State#state.connection, {send, ["PRIVMSG ", State#state.channel, " :", Msg]}).
send_msg(State, Msg) ->
    gen_server:cast(State#state.connection, {send, Msg}).
reply_ping(State, Data) ->
    gen_server:cast(State#state.connection, {send, ["PONG ", Data]}).
