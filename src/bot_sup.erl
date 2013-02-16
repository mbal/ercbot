-module(bot_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1]).
%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok, SetFile} = application:get_env(bot, settings_file),
    start_link(SetFile).

start_link(SettingsFile) ->
    ets:new(state_storage, [set, named_table, public]),
    utils:debug("~w starting...", [?MODULE]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [SettingsFile]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([SettingsFile]) ->
    {ok, { {one_for_one, 5, 10},
           [{config,
             {conf_server, start_link, [SettingsFile]},
             transient, 5000, worker, [conf_server]},

            {bot, 
             {bot_fsm, start_link, [self()]},
             transient, 5000, worker, [bot_fsm]}
            
           ]}}.
