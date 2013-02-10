-module(bot_sup).
-behaviour(supervisor).

%% API
-export([start_link/3]).
%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Nick, Channel, Server) ->
    ets:new(state_storage, [set, named_table, public]),
    utils:debug("~w starting...", [?MODULE]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Nick, Channel, Server]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Nick, Channel, Server]) ->
    {ok, { {one_for_one, 5, 10},
	   [{bot, 
	     {bot_fsm, start_link, [self(), Nick, Channel, Server]},
	     permanent,
	     5000,
	     worker,
	     [bot_fsm]
	     }]}}.

