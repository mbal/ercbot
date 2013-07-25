%%%-------------------------------------------------------------------
%%% @author mbal 
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 15 Feb 2013 by  mbal
%%%-------------------------------------------------------------------
-module(talk).

-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).

-export([name/0, help/0]).

name() ->
    "talk".
help() ->
    "generate some text through a Markov chain. Usage: "
    "%cmdstring% talk <length> [order]".

-define(SERVER, ?MODULE). 

-record(state, {freq_table, order}).

init([]) ->
    {ok, #state{freq_table=empty}}.

handle_event({cmd, Channel, _, "talk", [L, K]}, State) ->
    {Order, _} = string:to_integer(K),
    NewState = case (Order < 1) or (Order > 4) of
                   true -> 
                       irc_api:send_priv_msg(Channel, 
                                                "Order isn't in [1, 4]"),
                       State;
                   false ->
                       Tab = train(Order, State),
                       talk(Channel, L, Tab),
                       State#state{freq_table=Tab}
               end,
    {ok, NewState};

handle_event({cmd, Channel, _, "talk", [L]}, State) ->
    Tab = train(0, State),
    talk(Channel, L, Tab),
    {ok, State};
    
handle_event({cmd, Channel, _, "talk", []}, State) ->
    Tab = train(0, State),
    irc_api:send_priv_msg(Channel, markov:generate_text(100, Tab)),
    {ok, State};

handle_event(_Evt, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

train(0, State) ->
    case (State#state.freq_table /= empty) of
        true -> State#state.freq_table;
        false -> 
            TrainFileName = conf_server:lookup(train_file),
            markov:train(TrainFileName, 3, dict:new())
    end;
train(Order, State) ->
    case (State#state.order == Order) and (State#state.freq_table /= empty) of
        true -> 
            State#state.freq_table;
        false ->
            TrainFileName = conf_server:lookup(train_file),
            markov:train(TrainFileName, Order, dict:new())
    end.

talk(Channel, L, FreqTable) ->
    {Length, _} = string:to_integer(L),
    case (Length < 5) or (Length > 250) of
        true ->
            irc_api:send_priv_msg(Channel, "Lenght isn't in [5, 250]");
        false ->
            irc_api:send_priv_msg(Channel,
                                     markov:generate_text(Length, FreqTable))
    end.
