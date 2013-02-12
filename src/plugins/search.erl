-module(search).
-behaviour(gen_event).

-define(UA, "Mozilla/5.0 (ercbot)").
-define(WIKITEXT(ST, LANG), "Searching for " ++ ST ++ " on " ++ LANG ++ ".wikipedia.org").
-define(GOOGTEXT(ST), "Searching for " ++ ST ++ " on google.com").

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).
-export([name/0, short_description/0]).

name() -> "search".
short_description() -> "search wikipedia or google for the given term".

-record(state, {bot, table}).

init([Bot, _]) ->
    inets:start(),
    ReqTab = ets:new(request_table, [set]),
    {ok, #state{bot=Bot, table=ReqTab}}.

handle_event({cmd, Nick, "wiki", ["lang", Lang | Args]}, State) ->
    Term = string:join(Args, " "),
    wiki_search(Term, Lang, Nick, State#state.table),
    bot_fsm_api:send_priv_msg(State#state.bot, ?WIKITEXT(Term, Lang)),
    {ok, State};
handle_event({cmd, Nick, "wiki", Args}, State) ->
    Term = string:join(Args, " "),
    wiki_search(Term, "en", Nick, State#state.table),
    bot_fsm_api:send_priv_msg(State#state.bot, ?WIKITEXT(Term, "en")),
    {ok, State};
handle_event({cmd, Nick, "google", Args}, State) ->
    Term = string:join(Args, "+"),
    google_search(Term, Nick, State#state.table),
    bot_fsm_api:send_priv_msg(State#state.bot, ?GOOGTEXT(Term)),
    {ok, State};
handle_event(_Evt, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info({http, {ReqId, Response}}, State) ->
    [{ReqId, Nick, BackEnd}] = ets:lookup(State#state.table, ReqId),
    case BackEnd of
	wiki ->
	    case Response of
		{{_, 302, _}, Head, _} ->
		    Location = proplists:get_value("location", Head),
		    bot_fsm_api:send_priv_msg(State#state.bot, Nick ++ ", here's the URL you wanted: " ++ Location);
		_ -> 
		    bot_fsm_api:send_priv_msg(State#state.bot, Nick ++ ", I couldn't find anything")
	    end;
	google ->
	    case Response of
		{{_, 200, _}, _Head, Body} ->
		    {match, M} = re:run(Body, "<a href=(.*?)>(.*?)</a>", [global, {capture, all, list}]),
		    lists:foreach(fun(X) -> io:format("~p~n", [X]) end, M);

		_ -> bot_fsm_api:send_priv_msg(State#state.bot, Nick ++ ", encountered unexplicable error")
	    end
    end,
    ets:delete(State#state.table, ReqId),
    {ok, State};
handle_info(_Info, State) ->
    {ok, State}.
terminate(_Reason, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

wiki_search(Term, Lang, Nick, Table) ->
    Url = "http://" ++ Lang ++ ".wikipedia.org/w/index.php?search=" ++ http_uri:encode(Term),
    {ok, ReqId} = httpc:request(get, {Url, [{"User-Agent", ?UA}]}, [{autoredirect, false}], [{sync, false}]),
    ets:insert(Table, {ReqId, Nick, wiki}).
google_search(Term, Nick, Table) ->
    Url = "http://www.google.com/search?q=" ++ http_uri:encode(Term),
    {ok, ReqId} = httpc:request(get, {Url, [{"User-Agent", ?UA}]}, [], [{sync, false}]),
    ets:insert(Table, {ReqId, Nick, google}).
