%%%-------------------------------------------------------------------
%%% @author  <mbal>
%%% @copyright (C) 2013, 
%%% @doc
%%% Implementation from:
%%% http://pdincau.wordpress.com/2011/06/28/
%%% an-easy-way-to-handle-configuration-parameters-in-erlang/
%%% 
%%% @end
%%% Created : 14 Feb 2013 by  <mbal>
%%%-------------------------------------------------------------------
-module(conf_server).

-behaviour(gen_server).

%% API
-export([start_link/1, lookup/1, update/2, reload_config/0,
         write_back/0]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {filename, configuration}).

%%%===================================================================
%%% API
%%%===================================================================

lookup(Tag) ->
    gen_server:call(?SERVER, {lookup, Tag}).
update(Tag, Value) ->
    gen_server:call(?SERVER, {update, {Tag, Value}}).
reload_config() ->
    gen_server:cast(?SERVER, reload).
write_back() ->
    gen_server:cast(?SERVER, write_config).

start_link(FileName) ->
    utils:debug("~w starting...", [?MODULE]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [FileName], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([FName]) ->
    %%we need to trap exit signal in order to save the state
    %%so next time we can start with the updated configuration
    process_flag(trap_exit, true),
    {ok, Conf} = file:consult(FName),
    {ok, #state{configuration=Conf, filename=FName}}.

handle_call({lookup, Tag}, _From, State) ->
    Reply = case lists:keyfind(Tag, 1, State#state.configuration) of
                {Tag, Value} -> Value;
                false -> {error, no_such_setting}
            end,
    {reply, Reply, State};

handle_call({update, {Tag, Value}}, _From, State) ->
    NewConfig = lists:keyreplace(Tag, 1, State#state.configuration, 
                                 {Tag, Value}),
    {reply, ok, State#state{configuration=NewConfig}};

handle_call(_Req, _From, State) ->
    {reply, ok, State}.

handle_cast(reload, State) ->
    {ok, Conf} = file:consult(State#state.filename),
    {noreply, #state{configuration=Conf}};

handle_cast(write_config, State) ->
    write_config(State#state.filename, State#state.configuration),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    write_config(State#state.filename, State#state.configuration),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%PRIVATE FUNCTIONS

write_config(Filename, Config) ->
    {ok, FileObj} = file:open(Filename, [write]),
    lists:foreach(fun(X) -> 
                          file:write(FileObj, io_lib:fwrite("~p.\n", [X])) end,
                  Config).

