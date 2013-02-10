-module(utils).
-export([irc_parse/1, debug/1, debug/2]).
-export([reload/1, reload_all/0]).

irc_parse(Data) ->
    Tok = string:tokens(Data, ": "),
    tokens_parse(Tok).

tokens_parse([User, "PRIVMSG", _, CmdString, Text | Rest]) ->
    Nick = lists:nth(1, string:tokens(User, "!")),
    case CmdString == settings:cmd_string() of
	true -> parse_cmd(Nick, Text, Rest);
	false -> ok
    end;
tokens_parse([_, "376" | _]) ->
    {control, join};
tokens_parse(["PING" | Rest]) ->
    {control, ping, Rest};
tokens_parse(_) ->
    ok.

parse_cmd(Nick, Cmd, Args) ->
    Command = lists:nth(1, string:tokens(Cmd, "\r\n")),
    ArgList = lists:map(fun(X) -> lists:nth(1, string:tokens(X, "\r\n")) end, Args),
    {cmd, Nick, Command, ArgList}.

debug(Msg) ->
    io:format("[debug>]" ++ Msg ++ "~n").
debug(Msg, FmtArgs) ->
    io:format("[debug>]" ++ Msg ++ "~n", FmtArgs).

reload(M) ->
    code:purge(M),
    code:soft_purge(M),
    {module, M} = code:load_file(M),
    {ok, M}.

reload_all() ->
    Modules = [M || {M, P} <- code:all_loaded(), 
		    is_list(P) andalso string:str(P, "c:\\users\\utente\\ircb4\\ebin") > 0],
    [reload(M) || M <- Modules].
