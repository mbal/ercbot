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

-export([name/0, short_description/0]).

name() ->
    "talk".
short_description() ->
    "generate some text through a Markov chain".

-define(SERVER, ?MODULE). 

-record(state, {table}).

init([]) ->
    {ok, #state{}}.

handle_event({cmd, _, "talk", ["init", K]}, State) ->
    {Order, _} = string:to_integer(K),
    case (Order < 1) or (Order > 4) of
        true -> 
            plugin_api:send_priv_msg("Order isn't in [1, 4]"),
            NewState = #state{table=dict:new()};
        false ->
            NewState = #state{table=start_training(Order, State)}
    end,
    {ok, NewState};
handle_event({cmd, _, "talk", ["init"]}, State) ->
    NewState = #state{table=start_training(3, State)},
    {ok, NewState};

handle_event({cmd, _, "talk", [Len]}, State) ->
    {Length, _} = string:to_integer(Len),
    case (Length < 10) or (Length > 300) of
        true ->
            plugin_api:send_priv_msg("Length isn't in [10, 300]!");
        false ->
            plugin_api:send_priv_msg(generate_text(Length, State#state.table))
    end,
    {ok, State};

handle_event({cmd, _, "talk", []}, State) ->
    plugin_api:send_priv_msg(generate_text(100, State#state.table)),
    {ok, State};

handle_event(_Event, State) ->
    plugin_api:send_priv_msg("Usage: 1. !bot talk init <order> (only first time) 2."
                             " !bot talk <length>"),
    {ok, State}.

handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_training(Order, State) ->
    plugin_api:send_priv_msg("Starting training..."),
    TrainFileName = conf_server:lookup(train_file),
    Tab = markov:train(TrainFileName, Order, dict:new()),
    plugin_api:send_priv_msg("Training finished. Now you can use talk <length>!"),
    Tab.

generate_text(Length, Tab) ->
    markov:generate_text(Length, Tab).
