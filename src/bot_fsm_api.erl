-module(bot_fsm_api).
-export([change_nick/2, send_priv_msg/2, send_cmd_msg/2]).

change_nick(Bot, Nick) ->
    %change the state variable in irc_bot.erl
    gen_fsm:send_all_state_event(Bot, {change_nick, Nick}).
send_priv_msg(Bot, Msg) ->
    gen_fsm:send_all_state_event(Bot, {reply_priv, Msg}).
send_cmd_msg(Bot, Msg) ->
    gen_fsm:send_all_state_event(Bot, {reply_command, Msg}).

