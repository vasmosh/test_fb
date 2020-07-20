%%%-------------------------------------------------------------------
%% @doc test_fb top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(test_fb_sup).



-behaviour(supervisor).

-include("fb_macro.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    RestartStrategy = one_for_one, % one_for_one | one_for_all | rest_for_one
    Intensity = 10, %% max restarts
    Period = 1000, %% in period of time
    SupervisorSpecification = {RestartStrategy, Intensity, Period},

    {ok, MAX_NUM} =  application:get_env(?APPLICATION, max_num),
    {ok, Redis_Host} =  application:get_env(?APPLICATION, redis_host),
    {ok, Redis_Port} =  application:get_env(?APPLICATION, redis_port),
    {ok, Redis_DB} =  application:get_env(?APPLICATION, redis_db),
    {ok, Queue_Key} =  application:get_env(?APPLICATION, queue_key),
    {ok, Set_Key} =  application:get_env(?APPLICATION, result_set_key),

    Childs = [
        {gen_seq,{gen_seq, start_link, [MAX_NUM]},permanent,5000,worker,[gen_seq]},
        {filter_seq,{filter_seq, start_link, [MAX_NUM]},permanent,5000,worker,[filter_seq]},
        {bd,{bd, start_link, [{Redis_Host, Redis_Port, Redis_DB, Queue_Key, Set_Key}]},permanent,5000,worker,[bd]}
    ],
    {ok, {SupervisorSpecification, Childs}}.

%%====================================================================
%% Internal functions
%%====================================================================
