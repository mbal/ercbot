-module(bot_fsm).
-behaviour(gen_fsm).

-record(state, {supervisor, plugin_mgr, connection, nick, channels}).

-define(CHILD_SPEC(NAME, M, F, A), 
        {NAME,
         {M, F, A},
         transient,
         1000,
         worker,
         [M]}).

-export([start_link/1, init/1]).
-export([code_change/4, handle_event/3, handle_info/3]).
-export([handle_sync_event/4, terminate/3]).
-export([logged/2, idle/2, ready/2]).

start_link(Sup) ->
    utils:debug("~w starting...", [?MODULE]),
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [Sup], []).

init([Supervisor]) ->
    %% we should check if there is some saved state in `state_storage`
    %% calling the code to start the other processes (`irc_connector`
    %% and `plugin_mgr`) here, would cause a deadlock. So, we dispatch
    %% a message to ourselves, which will be taken care of just after
    %% init/1 (and start_link/0, obviously), finish.
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
    Res = utils:irc_parse(Data),
    case Res of
        {control, join} ->
            Channels = conf_server:lookup(channels),
            lists:foreach(fun(X) -> send_msg(State, ["JOIN :", X]) end,
                          Channels),
            {next_state, ready, State#state{channels=Channels}};
        {control, ping, Data} ->
            reply_ping(State, Data);
        {control, change_nick} ->
            Nick2 = conf_server:lookup(nick2),
            send_msg(State, ["NICK ", Nick2]),
            {next_state, logged, State#state{nick=Nick2}};
        _ -> {next_state, logged, State}
    end.

ready({recv, Msg}, State) ->
    Res = utils:irc_parse(Msg), 
    case Res of
        {control, ping, Data} ->
            reply_ping(State, Data);
        Message ->
            gen_server:cast(plugin_mgr, Message)
    end,
    {next_state, ready, State}.

code_change(_Old, _, _, _) -> 
    ok.

%% all the responses from the plugins match these functions
handle_event({reply_priv, Channel, Msg}, State, Data) -> 
    send_priv_msg(Data, Channel, Msg),
    {next_state, State, Data};

handle_event({send, Message}, State, Data) ->
    send_msg(Data, Message),
    {next_state, State, Data};

handle_event({join, Channel}, State, Data) ->
    send_msg(Data, ["JOIN ", Channel]),
    {next_state, State, Data#state{channels=[Channel | Data#state.channels]}};

handle_event({leave, Channel}, State, Data) ->
    send_msg(Data, ["PART ", Channel]),
    Channels = lists:delete(Channel, Data#state.channels),
    {next_state, State, Data#state{channels=Channels}};

handle_event({reply_command, Msg}, State, Data) -> 
    send_msg(Data, Msg),
    {next_state, State, Data};

handle_event({change_nick, NNick}, State, Data) ->
    send_msg(Data, ["NICK ", NNick]),
    conf_server:update(nick, NNick),
    {next_state, State, Data#state{nick=NNick}};

handle_event(restart, _State, Data) ->
    gen_server:cast(Data#state.plugin_mgr, terminate),
    Sup = Data#state.supervisor,
    ok = supervisor:terminate_child(Sup, plug_sup),
    ok = supervisor:delete_child(Sup, plug_sup),
    ok = supervisor:terminate_child(Sup, plug_mgr),
    ok = supervisor:delete_child(Sup, plug_mgr),
    ok = supervisor:terminate_child(Sup, irc_conn),
    ok = supervisor:delete_child(Sup, irc_conn),
    {stop, restart, Data};

handle_event(shutdown, _State, Data) ->
    {stop, shutdown, Data};

handle_event(Evt, State, Data) ->
    utils:debug("Unknown event ~w!", [Evt]),
    {next_state, State, Data}.

handle_info({start_connection, Sup}, StateName, State) ->
    ConPid = start_process(Sup, irc_conn, start_link, 
                           {irc_connector, [self()]}), 
    PlgPid = start_process(Sup, plug_mgr, start_link, 
                           {plugin_mgr, [State#state.supervisor]}),
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
            utils:debug("Encountered error, saving state... ~w and ~w", 
                        [CurrentState, CData]),
            ets:insert(state_storage, {?MODULE, CurrentState, CData})
    end,
    ok.

start_process(Sup, Name, F, {M, A}) ->
    case supervisor:start_child(Sup, ?CHILD_SPEC(Name, M, F, A)) of
        {ok, Pid} -> Pid;
        {error, {already_started, Pid}} ->
            gen_server:cast(Pid, {new_bot, self()}),
            Pid;
        _ -> ok
    end.

send_priv_msg(State, Channel, Msg) ->
    gen_server:cast(State#state.connection, 
                    {send, ["PRIVMSG ", Channel, " :", Msg]}).

send_msg(State, Msg) ->
    gen_server:cast(State#state.connection, {send, Msg}).

reply_ping(State, Data) ->
    gen_server:cast(State#state.connection, {send, ["PONG ", Data]}).
