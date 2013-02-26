%%%-------------------------------------------------------------------
%%% @author  mbal
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 26 Feb 2013 by  mbal
%%%-------------------------------------------------------------------
-module(admin).

-behaviour(gen_server).

%% API
-export([start_link/0, name/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {admin_list}).

%%%===================================================================
%%% API
%%%===================================================================

name() ->
    none.

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    case ets:lookup(state_storage, ?MODULE) of
        [{?MODULE, StateData}] ->
            {ok, StateData};
        [] ->
            Admins = conf_server:lookup(admin),
            {ok, #state{admin_list=Admins}}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({cmd, Channel, Nick, "admin", Args}, State) ->
    NewState = case lists:member(Nick, State#state.admin_list) of
                   true ->
                       handle_command(Args, Channel, State);
                   false ->
                       irc_api:send_priv_msg(Channel, "You're not on my list"),
                       State
    end,
    {noreply, NewState};
handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ets:insert(state_storage, {?MODULE, State}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_command(["add", User], _Channel, State) ->
    NewAdminList = [User | State#state.admin_list],
    conf_server:update(admin, NewAdminList),
    State#state{admin_list=NewAdminList};
handle_command(["rem", User], _Channel, State) ->
    NewAdminList = lists:delete(User, State#state.admin_list),
    conf_server:update(admin, NewAdminList),
    State#state{admin_list=NewAdminList};
handle_command(["list"], Channel, State) ->
    irc_api:send_priv_msg(Channel, get_admins(State#state.admin_list)),
    State;
handle_command(["cnick", NewNick], _Channel, State) ->
    irc_api:change_nick(NewNick),
    State;
handle_command(["restart"], Channel, State) ->
    irc_api:send_priv_msg(Channel, "I'll be back!"),
    irc_api:restart_bot(),
    State;
handle_command(["join", ChannelName], _Channel, State) ->
    case lists:nth(1, ChannelName) == $# of
        true ->
            irc_api:join_channel(ChannelName);
        false ->
            irc_api:join_channel([$# | ChannelName])
    end,
    State;
handle_command(["leave", ChannelName], _Channel, State) ->
    case lists:nth(1, ChannelName) == $# of
        true ->
            irc_api:leave_channel(ChannelName);
        false ->
            irc_api:leave_channel([$# | ChannelName])
    end,
    State;
handle_command(["shutdown"], _Channel, State) ->
    irc_api:shutdown_bot(),
    State;
handle_command(_, Channel, State) ->
    irc_api:send_priv_msg(Channel, "Unrecognized or incomplete option"),
    State.

get_admins(AdminList) -> string:join(AdminList, ", ").
