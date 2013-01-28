%%%-------------------------------------------------------------------
%%% @author Jeremy Ong <jeremy@playmesh.com>
%%% @copyright (C) 2012, PlayMesh, Inc.
%%%-------------------------------------------------------------------

-module(sharded_eredis_sup).

-behaviour(supervisor).

%% Include
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0, start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% API functions

start_link() ->
    {ok, Pools} = application:get_env(sharded_eredis, pools),
    {ok, GlobalOrLocal} = application:get_env(sharded_eredis, global_or_local),
    start_link(Pools, GlobalOrLocal).

start_link(Pools, GlobalOrLocal) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Pools, GlobalOrLocal]).

%% Supervisor callbacks

init([Pools, GlobalOrLocal]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = 5000,
    Type = worker,

    sharded_eredis_chash:store_ring(),

    PoolSpecs = lists:map(fun({PoolName, PoolConfig}) ->
                                  Args = [{name, {GlobalOrLocal, PoolName}},
                                          {worker_module, eredis}]
                                      ++ PoolConfig,
                                  {PoolName, {poolboy, start_link, [Args]},
                                   Restart, Shutdown, Type, []}
                          end, Pools),
    {ok, {SupFlags, PoolSpecs}}.
