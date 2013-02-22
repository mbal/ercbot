-module(wiki).
-behaviour(gen_event).

-define(UA, "Mozilla/5.0 (ercbot)").
-define(WIKITEXT(ST, LANG), ["Searching for ", ST, " on ", LANG, ".wikipedia.org"]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
        handle_info/2, terminate/2, code_change/3]).
-export([name/0, help/0]).

name() -> "wiki".
help() -> "search on wikipedia. Usage: "
              "%cmdstring% wiki [lang <language>] <query>.".

-record(state, {table}).

init([]) ->
    inets:start(),
    ReqTab = ets:new(request_table, [set]),
    {ok, #state{table=ReqTab}}.

handle_event({cmd, Channel, _Nick, "wiki", ["lang", Lang | Args]}, State) ->
    Term = string:join(Args, " "),
    wiki_search(Term, Lang, Channel, State#state.table),
    irc_api:send_priv_msg(Channel, ?WIKITEXT(Term, Lang)),
    {ok, State};

handle_event({cmd, Channel, _Nick, "wiki", Args}, State) ->
    Term = string:join(Args, " "),
    wiki_search(Term, "en", Channel, State#state.table),
    irc_api:send_priv_msg(Channel, ?WIKITEXT(Term, "en")),
    {ok, State};

handle_event(_Evt, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info({http, {ReqId, Response}}, State) ->
    case ets:lookup(State#state.table, ReqId) of
        [] -> ok;
        [{ReqId, Channel}] ->
            got_response(Response, Channel),
            ets:delete(State#state.table, ReqId)
    end,
    {ok, State};
handle_info(_Info, State) ->
    {ok, State}.
terminate(_Reason, State) ->
    ets:delete(State#state.table),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

wiki_search(Term, Lang, Channel, Table) ->
    Url = "http://" ++ Lang ++ ".wikipedia.org/w/index.php?search=" ++ 
        http_uri:encode(Term),
    {ok, ReqId} = httpc:request(get, {Url, [{"User-Agent", ?UA}]}, 
                                [{autoredirect, false}], [{sync, false}]),
    ets:insert(Table, {ReqId, Channel}).

got_response(Response, Channel) ->
    case Response of
        {{_, 302, _}, Head, _} ->
            Location = proplists:get_value("location", Head),
            irc_api:send_priv_msg(Channel, "=> " ++ Location);
        _ ->
            irc_api:send_priv_msg(Channel, "I couldn't find anything")
    end.
