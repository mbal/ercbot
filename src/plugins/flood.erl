%%%-------------------------------------------------------------------
%%% @author  <Utente@UTENTE-UTENTE>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 30 Apr 2013 by  <Utente@UTENTE-UTENTE>
%%%-------------------------------------------------------------------
-module(flood).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([cast/1, but_last/1]).
-export([name/0]).

-define(SERVER, ?MODULE). 

name() -> none.

%%% interval_table is an ets table, which contains, for every user, 
%%% a list of the times when the bot saw the last 7 messages
%%% counts is a list of {User, N} tuples, where N is the number of
%%% times the user flooded.
%%% min_interval is the user-definable interval between messages of
%%% the same user. If it passed less than `min_interval` between two
%%% messages, the user is warned and the count increased. Once
%%% the count reaches 3, the user is kicked.
-record(state, {interval_table, counts, min_interval}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

cast(Message) ->
    gen_server:cast(?SERVER, Message).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{interval_table=ets:new(users_table, [set]), min_interval=2, counts=[]}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_, _, State) ->
    {reply, ok, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
but_last(List) ->
    lists:reverse(but_last2(List, [])).
but_last2([], _) ->
    [];
but_last2([H], Acc) ->
    Acc;
but_last2([H|Tail], Acc) ->
    but_last2(Tail, [H|Acc]).

handle_cast({priv_msg, Nick, Channel, _Msg}, State) ->
    LastSeen = ets:lookup(State#state.interval_table, {Nick, Channel}),
    case LastSeen of
        [] ->
            Counts = State#state.counts,
            ets:insert(State#state.interval_table, {{Nick, Channel}, [erlang:now()]});
        [{{Nick, Channel}, PrevNow}] ->
            Times = [erlang:now() | PrevNow],
            case length(Times) == 6 of
                true ->
                    Diffs = lists:zipwith(fun(X, Y) -> timer:now_diff(X, Y) end, 
                                          but_last(Times), 
                                          tl(Times)),
                    Avg = lists:foldl(fun(X, Y) -> X + Y end, 0, Diffs) / length(Diffs),
                    case Avg < 1000000 * State#state.min_interval of
                        true ->
                            UserCount = lists:keyfind({Nick, Channel}, 1, State#state.counts),
                            case UserCount of
                                false ->
                                    irc_api:send_priv_msg(Channel, Nick ++ " please stop flooding! [1st warning]"),
                                    Counts = [{{Nick, Channel}, 1} | State#state.counts];
                                {_, 2} ->
                                    Counts = lists:keyreplace({Nick, Channel}, 1, State#state.counts, {{Nick, Channel}, 0}),
                                    irc_api:kick_user(Channel, Nick, "Flooding!");
                                {_, Count} ->
                                    Counts = lists:keyreplace({Nick, Channel}, 1, State#state.counts, {{Nick, Channel}, Count + 1}),
                                    irc_api:send_priv_msg(Channel, Nick ++ " please stop flooding! At the third warning you'll be kicked")
                            end;
                        false ->
                            Counts = State#state.counts,
                            ok
                    end,
                    NewTimes = but_last(PrevNow);
                false ->
                    NewTimes = Times,
                    Counts = State#state.counts,
                    ok
            end,
            ets:insert(State#state.interval_table, {{Nick, Channel}, NewTimes})
    end,
    {noreply, State#state{counts=Counts}};
handle_cast(_, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
