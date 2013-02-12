-module('8ball-it').
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-export([name/0, short_description/0]).

-record(state, {bot}).

name() ->
    "8ball".
short_description() ->
    "Ask a question, the magic 8ball foresees your future".

init([Bot, _]) ->
    {A, B, C} = now(),
    random:seed(A, B, C),
    {ok, #state{bot=Bot}}.

handle_event({cmd, _Nick, "8ball", []}, State) ->
    bot_fsm_api:send_priv_msg(State#state.bot, "Sai, perche' io ti risponda dovresti prima farmi una domanda"),
    {ok, State};
handle_event({cmd, _Nick, "8ball", _Args}, State) ->
    ReplyList = ["Per quanto posso vedere, si'", "E' certo", "E' decisamente cosi'",
		 "Molto probabilmente", "Le prospettive sono buone",
		 "I segni indicano di si'", "Senza alcun dubbio",
		 "Si'", "Si', definitivamente", "Ci puoi contare",
		 "E' difficile rispondere, prova ancora", 
		 "Rifai la domanda piu' tardi",
		 "Meglio che non risponda ora", "Non posso predirlo ora",
		 "concentrati e rifai la domanda", "non ci contare",
		 "La mia risposta e' no", "Le mie fonti dicono di no",
		 "Le prospettive non sono buone", "Molto incerto"],
    bot_fsm_api:send_priv_msg(State#state.bot, 
			      lists:nth(random:uniform(length(ReplyList)), 
					ReplyList)),
    {ok, State};
handle_event(_Evt, State) ->
    {ok, State}.

handle_call(_Call, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
