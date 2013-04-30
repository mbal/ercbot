-module(irc_api).

%% main irc commands
-export([send_priv_msg/2, change_nick/1, send_msg/1, kick_user/3]).
%% Bot related
-export([shutdown_bot/0, restart_bot/0]).
%% plugins managing
-export([load_plugin/1, remove_plugin/1, reload_plugins/0]).
%% control
-export([join_channel/1, leave_channel/1]).
%% misc utilities
-export([is_admin/1, is_channel/1]).

-define(BOT, bot_fsm).

kick_user(Channel, Nick, Reason) ->
    gen_fsm:send_all_state_event(?BOT, {send, "KICK " ++ Channel ++ " " ++ Nick ++ " " ++ Reason}).

send_priv_msg(Channel, Message) ->
    gen_fsm:send_all_state_event(?BOT, {reply_priv, Channel, Message}).

join_channel(Channel) ->
    gen_fsm:send_all_state_event(?BOT, {join, Channel}).

leave_channel(Channel) ->
    gen_fsm:send_all_state_event(?BOT, {leave, Channel}).

send_msg(Message) ->
    gen_fsm:send_all_state_event(?BOT, {send, Message}).

change_nick(NewNick) ->
    gen_fsm:send_all_state_event(?BOT, {change_nick, NewNick}).

restart_bot() ->
    gen_fsm:send_all_state_event(?BOT, restart).

shutdown_bot() ->
    gen_fsm:send_all_state_event(?BOT, shutdown).

reload_plugins() ->
    gen_server:call(plugin_mgr, reload).

%% load_plugin(PluginName :: string()) -> error | ok.
%% error means that the plugin `PluginName` is either already loaded
%% or is not in the list `plugins` loaded from the conf_server.
load_plugin(PluginName) ->
    gen_server:call(plugin_mgr, {reload, PluginName}).

%% remove_plugin(PluginName :: string()) -> error | ok.
%% error is reported when there isn't a plugin with that name
remove_plugin(PluginName) ->
    gen_server:call(plugin_mgr, {remove, PluginName}).

%% is_admin(string()) -> true | false.
is_admin(Name) ->
    lists:member(Name, conf_server:lookup(admin)).

%% is_channel(string()) -> true | false.
is_channel([$# | _Rest]) -> true;
is_channel(_)            -> false.
