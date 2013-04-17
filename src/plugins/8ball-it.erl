-module('8ball-it').
-behaviour(gen_event).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
        handle_info/2, terminate/2, code_change/3]).

-export([name/0, help/0]).

name() ->
    "8ball".
help() ->
    "Ask a question, the magic 8ball foresees your future".

init([]) ->
    {A, B, C} = now(),
    random:seed(A, B, C),
    {ok, {}}.

handle_event({cmd, Channel, _Nick, "8ball", []}, State) ->
    irc_api:send_priv_msg(Channel, "Sai, perche' io ti risponda "
        "dovresti prima farmi una domanda"),
    {ok, State};
handle_event({cmd, Channel, _Nick, "8ball", _Args}, State) ->
    ReplyList = ["Per quanto posso vedere, si'", "E' certo", 
        "E' decisamente cosi'", "Rifai la domanda piu' tardi",
        "Molto probabilmente", "Le prospettive sono buone",
        "I segni indicano di si'", "Senza alcun dubbio",
        "Si'", "Si', definitivamente", "Ci puoi contare",
        "E' difficile rispondere, prova ancora", 
        "Meglio che non risponda ora", "Non posso predirlo ora",
        "concentrati e rifai la domanda", "non ci contare",
        "La mia risposta e' no", "Le mie fonti dicono di no",
        "Le prospettive non sono buone", "Molto incerto"],
    irc_api:send_priv_msg(Channel, utils:choice(ReplyList)), 
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
