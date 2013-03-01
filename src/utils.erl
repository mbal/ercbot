-module(utils).
-export([irc_parse/1, debug/1, debug/2, choice/1, mtokens/3]).

irc_parse(Data) ->
    Toks = mtokens(Data, $:, 2),
    Tok = lists:flatmap(fun(X) -> string:tokens(X, " ") end, Toks),
    tokens_parse(Tok).

%%% if CmdString = "!" should:
%%% a. !time
%%% b. ! time
%%% be accepted? Or both?
%%% if CmdString = "!bot", which one should be accepted?
%%% a. !bottime
%%% b. !bot time
%%% The code that accept all the four situation is the following: 
%%% tokens_parse([User, "PRIVMSG", _Channel | Rest]) ->
%%%     CmdString = conf_server:lookup(cmd_string),
%%%     Nick = lists:nth(1, string:tokens(User, "!")),
%%%     FirstWord = lists:nth(1, Rest),
%%%     case safter(FirstWord, CmdString) of
%%%         false -> 
%%%             %% could be either ctcp command or privmsg
%%%             Message = string:join(Rest, " "),
%%%             case ctcp_parse(Message) of
%%%                 {Command, Data} -> {ctcp, Command, Data};
%%%                 false -> {priv_msg, Message}
%%%             end;
%%%         [] ->
%%%             %% this happens when the `CmdString` is followed by a space.
%%%             %% like: "! time": I consider it legal, even though it's borderline
%%%             %% We must accept it because if CmdString = "!bot", its normal use
%%%             %% would be a command like "!bot time", instead of "!bottime".
%%%             case parse_cmd(Nick, tl(Rest)) of
%%%                 %% if you send `CmdString` on a line alone, it would be caught
%%%                 %% even here, but it's a PRIVMSG, not a CMD. We must handle 
%%%                 %% this special case separately.
%%%                 priv_msg -> {priv_msg, CmdString};
%%%                 Other -> Other
%%%             end;
%%%         String ->
%%%             %% this matches in cases like: "!time" or "!bottime" (when the 
%%%             %% CmdString is !bot).This last example is not really nice, but, 
%%%             %% since we accept the first, we must accept every similar pattern.
%%%             %% If you *really* want a space between CmdString and Command, 
%%%             %% change the following line to this: `parse_cmd(Nick, tl(Rest))`
%%%             %% NOTE: I accept suggestions on how to fix this "inconsistent"
%%%             %% behaviour. Which cases should it accept?
%%%             parse_cmd(Nick, [String | tl(Rest)]);
%%%     end;
%%% 
%%% My temporary solution, instead, handles the a. situation when the CmdString is a single letter
%%% and the b. when CmdString is a string. This is probably better.

tokens_parse([User, "PRIVMSG", Channel | Rest]) ->
    CmdString = conf_server:lookup(cmd_string),
    Nick = lists:nth(1, string:tokens(User, "!")),
    FirstWord = lists:nth(1, Rest),
    case safter(FirstWord, CmdString) of
        false -> 
            %% could be either ctcp command or privmsg
            Message = string:join(Rest, " "),
            case ctcp_parse(Message) of
                {Command, Data} -> {ctcp, Nick, Command, Data};
                false -> {priv_msg, Nick, Channel, Message}
            end;
        [] when length(CmdString) > 1 ->
            case parse_cmd(Nick, Channel, tl(Rest)) of
                %% if you send `CmdString` on a line alone, it would be caught
                %% even here, but it's a PRIVMSG, not a CMD. We must handle 
                %% this special case separately.
                priv_msg -> {priv_msg, Nick, Channel, CmdString};
                Other -> Other
            end;
        String when length(CmdString) == 1, length(String) > 0 ->
            %% this matches only when CmdString is a single letter, and the
            %% command is complete e.g. !time, but not when a single ! is sent.
            %% note that this latter situation can match here since ! doesn't
            %% match the previous clause, even though safter(X, "!") = [].
            parse_cmd(Nick, Channel, [String | tl(Rest)]);
        _ ->
            {priv_msg, Nick, Channel, string:join(Rest, " ")}
    end;
tokens_parse([User, "PART", Channel]) ->
    {control, user_quit, User, Channel};
tokens_parse([User, "QUIT" | _Rest]) ->
    {control, user_quit, User};
tokens_parse([User, "JOIN", Channel]) ->
    {control, user_join, User, Channel};
tokens_parse([User, "NICK", NewNick]) ->
    {control, user_nick, User, NewNick};
tokens_parse([_, "KICK", From, Who, _Reason]) ->
    {control, user_kick, Who, From};

tokens_parse([_, "353", _, _, Channel | UserList]) ->
    {control, user_list, Channel, UserList};
tokens_parse([_, "366", _, Channel | _]) ->
    {control, user_end, Channel};

tokens_parse([_, "376" | _]) ->
    {control, join};
tokens_parse([_, "433" | _]) ->
    {control, change_nick}; 
tokens_parse(["PING" | Rest]) ->
    {control, ping, Rest};
tokens_parse(_What) ->
    ok.

ctcp_parse(Cmd) ->
    %% I can't seem to get a nice pattern for the function definition
    %% that always works, so fuck it, I'll use a regular expression!
    case re:run(Cmd, "^\x01([a-zA-Z]*) ?([a-zA-Z0-9]*)?\x01$",
                [global, {capture, all_but_first, list}]) of
        nomatch -> false;
        {match, [[FirstG, SecondG]]} -> {FirstG, SecondG}
    end.

parse_cmd(_, _, []) ->
    priv_msg;
parse_cmd(Nick, Channel, [Cmd|Args]) ->
    Command = lists:nth(1, string:tokens(Cmd, "\r\n")),
    ArgList = lists:map(fun(X) -> lists:nth(1, string:tokens(X, "\r\n")) end, Args),
    {cmd, Channel, Nick, Command, ArgList}.

debug(Msg) ->
    io:format("[debug>]" ++ Msg ++ "~n").
debug(Msg, FmtArgs) ->
    io:format("[debug>]" ++ Msg ++ "~n", FmtArgs).

%%--------------------------------------------------------------------
%% @doc
%% returns the suffix of String if Prefix is the prefix of String
%% false otherwise.
%% suffix("string", "st") -> "ring";
%% suffix("string", "ab") -> false;
%% suffix("string", "string") -> [];
%% @spec
%% safter(String :: string(), Prefix :: string()) -> false | string()
%% @end
%%--------------------------------------------------------------------

-spec safter(String :: string(), Prefix :: string()) -> false | string().

safter(String, []) -> 
    String;
safter([], _) ->
    false;
safter([Head|String], [Head|Prefix]) ->
    safter(String, Prefix);
safter(_, _) ->
    false.

%%--------------------------------------------------------------------
%% @doc
%% the same as string:tokens, but takes three parameters, the last 
%% being the maximum number of splits. If `MaxSplit` is negative,
%% the result is the same as string:tokens(String, Sep). The only
%% difference is that mtokens takes a char() as separator, while
%% tokens can use a string.
%% @spec
%% tokens(string(), char(), int()) -> [string()].
%% @end
%%--------------------------------------------------------------------
-spec mtokens(string(), char(), integer()) -> [string()].
mtokens(String, Separator, MaxSplit) ->
    tokens1(String, Separator, MaxSplit, []).

tokens1([], _, 0, Acc) ->
    lists:reverse(Acc);
tokens1([], _, _, Acc) ->
    lists:reverse(Acc);

tokens1(String, _, 0, Acc) ->
    lists:reverse(Acc) ++ [String];

tokens1([H | Rest], Separator, MaxSplit, Acc) ->
    case (H == Separator) of
        true ->
            tokens1(Rest, Separator, MaxSplit-1, Acc);
        false ->
            find_next_sep(Rest, Separator, MaxSplit, Acc, [H])
    end.

find_next_sep([], _, _, Acc, StrAcc) ->
    lists:reverse([lists:reverse(StrAcc) | Acc]);
find_next_sep([H|String], Sep, MaxSplit, Acc, StrAcc) ->
    case (H == Sep) of
        true ->
            tokens1(String, Sep, MaxSplit-1, [lists:reverse(StrAcc) | Acc]);
        false ->
            find_next_sep(String, Sep, MaxSplit, Acc, [H | StrAcc])
    end.

%%--------------------------------------------------------------------
%% @doc
%% The same as Python's random.choice. Returns a random element from
%% the list
%% @spec
%% choice([T]) -> T.
%% @end
%%--------------------------------------------------------------------
choice(List) ->
    lists:nth(random:uniform(length(List)), List).
