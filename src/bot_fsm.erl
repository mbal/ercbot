-module(bot_fsm).
-behaviour(gen_fsm).

-record(state, {supervisor, plugin_mgr, connection, nick, channel}).

-define(CHILD_SPEC(NAME, M, F, A), 
        {NAME,
         {M, F, A},
         transient,
         1000,
         worker,
         [M]}).

-export([start_link/1,  init/1]).
-export([code_change/4, handle_event/3, handle_info/3]).
-export([handle_sync_event/4, terminate/3]).
-export([logged/2, idle/2, ready/2]).

start_link(Sup) ->
    utils:debug("~w starting...", [?MODULE]),
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [Sup], []).

init([Supervisor]) ->
    %%we should check if there is some saved state in `state_storage`
    self() ! {start_connection, Supervisor},
    case ets:lookup(state_storage, ?MODULE) of
        [{?MODULE, NextState, NextData}] ->
            {ok, NextState, NextData#state{supervisor=Supervisor}};
        [] ->
            {ok, idle, #state{supervisor=Supervisor}}
    end.

idle(connected, State) ->
    Nick = conf_server:lookup(nick),
    Whois = conf_server:lookup(whois),
    send_msg(State, ["NICK ", Nick]),
    send_msg(State, ["USER ", Nick, " 8 * : ", Whois]),
    {next_state, logged, State#state{nick=Nick}};

idle(_, State) ->
    {next_state, idle, State}.

logged({recv, Data}, State) ->
    Res = utils:irc_parse(Data), %string:tokens(Data, ": ")),
    case Res of
        {control, join} ->
            Channel = conf_server:lookup(channel),
            send_msg(State, ["JOIN :", Channel]),
            {next_state, ready, State#state{channel=Channel}};
        {control, ping, Data} ->
            utils:debug("Received PING, replying"),
            reply_ping(State, Data);
        {control, change_nick} ->
            %%TODO: add code to change the nick
            {next_state, logged, State};
        _ -> {next_state, logged, State}
    end.

ready({recv, Msg}, State) ->
    Res = utils:irc_parse(Msg), 
    case Res of
        %%TODO: delete these two lines
        {cmd, _Nick, "crash", _Args} ->
            _ = 1/0;
        {cmd, Nick, Cmd, Args} -> 
            io:format("got command"),
            gen_server:cast(State#state.plugin_mgr, {cmd, Nick, Cmd, Args});
        {control, ping, Data} ->
            utils:debug("Received PING, replying"),
            reply_ping(State, Data);
        _ -> ok
    end,
    {next_state, ready, State}.

code_change(_Old, _, _, _) -> 
    utils:debug("Code change event received"),
    ok.

%%this collects all the responses from the plugins.
%%handle_event(shutdown, _State, Data) ->
%%    gen_server:cast(terminate, Data#state.pluginmgr),
%%    ok = supervisor:terminate_child(Data#state.supervisor, irc_plug),
%%    ok = supervisor:terminate_child(Data#state.supervisor, irc_conn),
%%    {stop, shutdown, Data};
handle_event({reply_priv, Msg}, State, Data) -> 
    send_priv_msg(Data, Msg),
    {next_state, State, Data};

handle_event({reply_command, Msg}, State, Data) -> 
    send_msg(Data, Msg),
    {next_state, State, Data};

handle_event({change_nick, NNick}, State, Data) ->
    send_msg(Data, ["NICK ", NNick]),
    conf_server:update(nick, NNick),
    {next_state, State, Data#state{nick=NNick}};

handle_event(restart, _State, Data) ->
    ok = supervisor:terminate_child(Data#state.supervisor, irc_conn),
    ok = supervisor:delete_child(Data#state.supervisor, irc_conn),

    gen_server:cast(Data#state.plugin_mgr, terminate),

    ok = supervisor:terminate_child(Data#state.supervisor, irc_plug),
    ok = supervisor:delete_child(Data#state.supervisor, irc_plug),
    {stop, restart, Data};

handle_event(shutdown, _State, Data) ->
    ok = supervisor:terminate_child(Data#state.supervisor, irc_conn),
    ok = supervisor:delete_child(Data#state.supervisor, irc_conn),
    gen_server:cast(Data#state.plugin_mgr, terminate),
    ok = supervisor:terminate_child(Data#state.supervisor, irc_plug),
    ok = supervisor:delete_child(Data#state.supervisor, irc_plug),
    {stop, shutdown, Data};

handle_event(Evt, State, Data) ->
    utils:debug("Unknown event ~w!", [Evt]),
    {next_state, State, Data}.

handle_info({start_connection, Sup}, StateName, State) ->
    io:format("Starting children"),
    io:format("~w", [supervisor:which_children(State#state.supervisor)]),
    ConPid = start_process(Sup, irc_conn, start_link, {irc_connector, [self(), Sup]}), 
    PlgPid = start_process(Sup, irc_plug, start_link, {plugin_mgr, [self()]}),
    {next_state, StateName, State#state{plugin_mgr=PlgPid, connection=ConPid}};

handle_info(_Msg, State, Data) -> 
    {next_state, State, Data}.

handle_sync_event(_Event, _From, StateName, Data) -> 
    {next_state, StateName, Data}.

terminate(Reason, CurrentState, CData) ->
    case Reason of
        restart ->
            ets:insert(state_storage, {?MODULE, idle, CData}),
            utils:debug("got `restart` message~n");
        shutdown ->
            utils:debug("got `shutdown` message~n"),
            application:stop(bot);
        _ -> 
            utils:debug("Encountered error, saving state... ~w and ~w ~w", [Reason, CurrentState, CData]),
            send_priv_msg(CData, "Bot encountered an error, restarting..."),
            ets:insert(state_storage, {?MODULE, CurrentState, CData})
    end,
    ok.

start_process(Sup, Name, F, {M, A}) ->
    case supervisor:start_child(Sup, ?CHILD_SPEC(Name, M, F, A)) of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} ->
            gen_server:cast(Pid, {new_bot, self()}),
            Pid;
        What -> io:format("errore ~w", [What]),
                ok
    end.

send_priv_msg(State, Msg) ->
    gen_server:cast(State#state.connection, {send, ["PRIVMSG ", State#state.channel, " :", Msg]}).
send_msg(State, Msg) ->
    gen_server:cast(State#state.connection, {send, Msg}).
reply_ping(State, Data) ->
    gen_server:cast(State#state.connection, {send, ["PONG ", Data]}).
