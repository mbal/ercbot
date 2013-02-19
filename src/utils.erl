-module(utils).
-export([irc_parse/1, debug/1, debug/2]).

irc_parse(Data) ->
    Tok = string:tokens(Data, ": "),
    tokens_parse(Tok).

tokens_parse([User, "PRIVMSG", _Channel | Rest]) ->
    %% right now, Channel isn't really important, since the
    %% bot can listen on a single channel.
    FirstWord = lists:nth(1, Rest),
    CmdString = conf_server:lookup(cmd_string),
    Nick = lists:nth(1, string:tokens(User, "!")),
    case CmdString == FirstWord of
        true -> 
            parse_cmd(Nick, tl(Rest));
        false -> 
            %% at this point the message could either be a 
            %% CTCP message or a real PRIVMSG
            Message = string:join(Rest, " "),
            case ctcp_parse(Message) of
                {Command, Data} -> {ctcp, Command, Data};
                false -> {priv_msg, Message}
            end
    end;
tokens_parse([User, "PART", _Channel]) ->
    {control, user_quit, User};
tokens_parse([User, "QUIT", _Channel]) ->
    {control, user_quit, User};
tokens_parse([User, "JOIN", _Channel]) ->
    {control, user_join, User};

tokens_parse([_, "353", _, _, _Channel | UserList]) ->
    {control, user_list, UserList};
tokens_parse([_, "366" | _]) ->
    {control, user_end};

tokens_parse([_, "376" | _]) ->
    {control, join};
tokens_parse([_, "433" | _]) ->
    {control, change_nick}; 
tokens_parse(["PING" | Rest]) ->
    {control, ping, Rest};
tokens_parse(_) ->
    ok.

ctcp_parse(Cmd) ->
    %% I can't seem to get a nice pattern for the function 
    %% definition that always works, so fuck it, 
    %% I'll use a regular expression!
    case re:run(Cmd, "^\x01([a-zA-Z]*) ?([a-zA-Z0-9]*)?\x01$",
                [global, {capture, all_but_first, list}]) of
        nomatch -> false;
        {match, [[FirstG, SecondG]]} -> {FirstG, SecondG}
    end.

parse_cmd(Nick, [Cmd|Args]) ->
    Command = lists:nth(1, string:tokens(Cmd, "\r\n")),
    ArgList = lists:map(fun(X) -> lists:nth(1, string:tokens(X, "\r\n")) end, Args),
    {cmd, Nick, Command, ArgList}.

debug(Msg) ->
    io:format("[debug>]" ++ Msg ++ "~n").
debug(Msg, FmtArgs) ->
    io:format("[debug>]" ++ Msg ++ "~n", FmtArgs).
