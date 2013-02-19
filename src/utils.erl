-module(utils).
-export([irc_parse/1, debug/1, debug/2]).

irc_parse(Data) ->
    Tok = string:tokens(Data, ": "),
    tokens_parse(Tok).

%%hideous kludge, but I cannot think of a better way.
tokens_parse([User, "PRIVMSG", _, CmdString, Text | Rest]) ->
    Nick = lists:nth(1, string:tokens(User, "!")),
    String = conf_server:lookup(cmd_string),
    case CmdString == String of
        true -> parse_cmd(Nick, Text, Rest);
        false -> ok
    end;
tokens_parse([_, "353", _, _, Channel | UserList]) ->
    %%first char of UserList is :
    {control, user_list, Channel, UserList};
tokens_parse([_, "376" | _]) ->
    {control, join};
tokens_parse([_, "433" | _]) ->
    {control, change_nick}; 
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
