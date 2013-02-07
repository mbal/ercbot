-module(irc_supervisor).
-behaviour(supervisor).

-export([start_link/3, init/1]).

%start_link() ->
%    ets:new(state_storage, [set, named_table, public]),
%    utils:debug("~w starting...", [?MODULE]),
%    supervisor:start_link({local, ?MODULE}, ?MODULE, [settings:nick(),
%            settings:channel(), settings:server()]).

start_link(Nick, Channel, Server) ->
    ets:new(state_storage, [set, named_table, public]),
    utils:debug("~w starting...", [?MODULE]),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Nick, Channel, Server]).

init([Nick, Channel, Server]) ->
    {ok, {{one_for_one, 4, 10}, [{ibot, 
            {irc_bot, start_link, [self(), Nick, Channel, Server]}, 
            permanent, 
            4000,
            worker, 
            [irc_bot]
        }]}}.

%init(_Args) ->
%    init([settings:nick(), settings:channel(), settings:sever()]).

