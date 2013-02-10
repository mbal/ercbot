-module(bot_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(normal, []) ->
    bot_sup:start_link(settings:nick(), settings:channel(),
			      settings:server());
start(normal, [Nick, Channel, Server]) ->
    bot_sup:start_link(Nick, Channel, Server).

stop(_State) ->
    ok.
