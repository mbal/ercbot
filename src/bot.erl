-module(bot).
-behaviour(application).
-export([start/2, stop/1]).

start(normal, []) ->
    %load the settings from settings.erl
    irc_supervisor:start_link(settings:nick(), settings:channel(),
        settings:server());
start(normal, [Nick, Channel, Server]) ->
    irc_supervisor:start_link(Nick, Channel, Server).

stop(_State) ->
    ok.
