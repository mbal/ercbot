-module(bot_app).
-behaviour(application).
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(normal, []) ->
    bot_sup:start_link();
start(normal, [SettingsFile]) ->
    bot_sup:start_link(SettingsFile).

stop(_State) ->
    ok.
