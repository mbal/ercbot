-module(bot).
-export([start/0, start/1, stop/0]).

%%% start/0 should be called only if the settings_file environment variable
%%% was set (in a .config file or passed in the command line)
%%%
%%% $ cat start_conf.config
%%%  [{bot, [{settings_file, "path/to/settings.cfg"}]}].
%%% $ erl -pa ebin -config start_conf
%%%
%%% or
%%%
%%% $ erl -bot settings_file '"path/to/settings.cfg"' -pa ebin
%%% (note that there are both ' and ").
%%% and then, in both cases, bot:start().

start() ->
    application:start(bot).

start(SettingsFile) ->
    application:set_env(bot, settings_file, SettingsFile),
    application:start(bot).

stop() ->
    application:stop(bot).
