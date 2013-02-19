-module(utils).
-export([irc_parse/1, debug/1, debug/2]).

irc_parse(Data) ->
    Tok = string:tokens(Data, ": "),
    tokens_parse(Tok).

tokens_parse([User, "PRIVMSG", Channel | Rest]) ->
    FirstWord = lists:nth(1, Rest),
    CmdString = conf_server:lookup(cmd_string),
    Nick = lists:nth(1, string:tokens(User, "!")),
    case CmdString == FirstWord of
        true -> parse_cmd(Nick, tl(Rest));
        false -> {priv_msg, string:join(Rest, " ")} 
    end;
tokens_parse([_, "353", _, _, Channel | UserList]) ->
    {control, user_list, Channel, UserList};
tokens_parse([_, "376" | _]) ->
    {control, join};
tokens_parse([_, "433" | _]) ->
    {control, change_nick}; 
tokens_parse(["PING" | Rest]) ->
    {control, ping, Rest};
tokens_parse(_) ->
    ok.

parse_cmd(Nick, [Cmd|Args]) ->
    Command = lists:nth(1, string:tokens(Cmd, "\r\n")),
    ArgList = lists:map(fun(X) -> lists:nth(1, string:tokens(X, "\r\n")) end, Args),
    {cmd, Nick, Command, ArgList}.

debug(Msg) ->
    io:format("[debug>]" ++ Msg ++ "~n").
debug(Msg, FmtArgs) ->
    io:format("[debug>]" ++ Msg ++ "~n", FmtArgs).
