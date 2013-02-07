-module(utils).
-define(BOT_CMD, settings:cmd_string()).

-export([irc_parse/1, debug/1, debug/2]).

irc_parse([User, "PRIVMSG", _, CmdString, Command]) ->
    case CmdString == ?BOT_CMD of
        true ->
            Nick = lists:nth(1, string:tokens(User, "!")),
            Cmd = lists:nth(1, string:tokens(Command, "\r\n")),
            {cmd, Nick, Cmd, []};
        false -> ok
    end;

irc_parse([User, "PRIVMSG", _, CmdString, Command | Rest]) ->
    case CmdString == ?BOT_CMD of
        true ->
            Nick = lists:nth(1, string:tokens(User, "!")),
            Cmd = lists:nth(1, string:tokens(Command, "\r\n")),
            Rst = lists:map(fun(X) -> lists:nth(1, string:tokens(X, "\r\n")) end, Rest),
            {cmd, Nick, Cmd, Rst};
        _ -> ok
    end;
irc_parse([_, "376" | _]) ->
    debug("Matching join event"),
    {control, join};
irc_parse(["PING"|Rest]) ->
    {control, ping, Rest};
irc_parse(_) -> ok.

debug(Msg) ->
    io:format("[debug>]" ++ Msg ++ "~n").
debug(Msg, FmtArgs) ->
    io:format("[debug>]" ++ Msg ++ "~n", FmtArgs).
