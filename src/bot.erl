-module(bot).
-export([start/1, stop/0]).

start(SettingsFile) ->
    application:set_env(bot, settings_file, SettingsFile),
    application:start(bot).

stop() ->
    application:stop(bot).
