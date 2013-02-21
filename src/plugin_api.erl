-module(plugin_api).
-export([send_priv_msg/2, change_nick/1, send_msg/1]).
-export([shutdown_bot/0, restart_bot/0, reload_plugins/0]).

-define(BOT, bot_fsm).

send_priv_msg(Channel, Message) ->
    gen_fsm:send_all_state_event(?BOT, {reply_priv, Channel, Message}).

send_msg(Message) ->
    gen_fsm:send_all_state_event(?BOT, {send, Message}).

change_nick(NewNick) ->
    gen_fsm:send_all_state_event(?BOT, {change_nick, NewNick}).

restart_bot() ->
    gen_fsm:send_all_state_event(?BOT, restart).

shutdown_bot() ->
    gen_fsm:send_all_state_event(?BOT, shutdown).

reload_plugins() ->
    gen_server:cast(plugin_mgr, reload).

