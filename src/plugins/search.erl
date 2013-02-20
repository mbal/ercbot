-module(search).
-behaviour(gen_event).

-define(UA, "Mozilla/5.0 (ercbot)").
-define(WIKITEXT(ST, LANG), "Searching for " ++ ST ++ " on " ++ LANG ++ ".wikipedia.org").
-define(GOOGTEXT(ST), "Searching for " ++ ST ++ " on google.com").

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
         handle_info/2, terminate/2, code_change/3]).
-export([name/0, short_description/0]).

name() -> "wiki/google".
short_description() -> "search wikipedia or google for the given term".

-record(state, {table}).

init([]) ->
    inets:start(),
    ReqTab = ets:new(request_table, [set]),
    {ok, #state{table=ReqTab}}.

handle_event({cmd, Channel, _Nick, "wiki", ["lang", Lang | Args]}, State) ->
    Term = string:join(Args, " "),
    wiki_search(Term, Lang, Channel, State#state.table),
    plugin_api:send_priv_msg(Channel, ?WIKITEXT(Term, Lang)),
    {ok, State};
handle_event({cmd, Channel, _Nick, "wiki", Args}, State) ->
    Term = string:join(Args, " "),
    wiki_search(Term, "en", Channel, State#state.table),
    plugin_api:send_priv_msg(Channel, ?WIKITEXT(Term, "en")),
    {ok, State};
handle_event({cmd, Channel, _Nick, "google", Args}, State) ->
    Term = string:join(Args, "+"),
    google_search(Term, Channel, State#state.table),
    plugin_api:send_priv_msg(Channel, ?GOOGTEXT(Term)),
    {ok, State};
handle_event(_Evt, State) ->
    {ok, State}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info({http, {ReqId, Response}}, State) ->
    [{ReqId, Channel, BackEnd}] = ets:lookup(State#state.table, ReqId),
    case BackEnd of
        wiki ->
            case Response of
                {{_, 302, _}, Head, _} ->
                    Location = proplists:get_value("location", Head),
                    plugin_api:send_priv_msg(Channel, "here's the URL you wanted: " ++ Location);
                _ -> 
                    plugin_api:send_priv_msg(Channel, "I couldn't find anything")
            end;
        google ->
            case Response of
                {{_, 200, _}, _Head, Body} ->
                    Results = parse_google_results(Body, 3),
                    plugin_api:send_priv_msg(Channel, "First 3 result for your query: "),
                    lists:foreach(fun([X]) -> plugin_api:send_priv_msg(Channel, X) end, Results);
                _ -> plugin_api:send_priv_msg(Channel, "Encountered unexplicable error")
            end
    end,
    ets:delete(State#state.table, ReqId),
    {ok, State};
handle_info(_Info, State) ->
    {ok, State}.
terminate(_Reason, State) ->
    ets:delete(State#state.table),
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

wiki_search(Term, Lang, Channel, Table) ->
    Url = "http://" ++ Lang ++ ".wikipedia.org/w/index.php?search=" ++ http_uri:encode(Term),
    {ok, ReqId} = httpc:request(get, {Url, [{"User-Agent", ?UA}]}, 
                                [{autoredirect, false}], [{sync, false}]),
    ets:insert(Table, {ReqId, Channel, wiki}).
google_search(Term, Channel, Table) ->
    Url = "http://www.google.com/search?q=" ++ http_uri:encode(Term),
    {ok, ReqId} = httpc:request(get, {Url, [{"User-Agent", ?UA}]}, [], 
                                [{sync, false}]),
    ets:insert(Table, {ReqId, Channel, google}).

parse_google_results(WebPage, N) ->
%%%kids, don't try this at home! Parsing html with regexes is bad.
    {match, Res} = re:run(WebPage, "<li class=\"g\".*?><div class=.*?><h3 "
                          "class=\"r\".*?><a.*?href=\"/url\\?q=(.*?)"
                          "&amp;(?:.*?)\">.*?</a></h3>.*?</li>", 
                          [global, {capture, all_but_first, list}]),
    lists:sublist(Res, N).

