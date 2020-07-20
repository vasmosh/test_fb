%%%-------------------------------------------------------------------
%%% @author mva
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jul 2020 14:10
%%%-------------------------------------------------------------------
-module(bd).
-author("mva").

-behaviour(gen_server).

%% API
-export([start_link/1]).

-export([save_seq/1, save_set/1]).
-export([show_seq/0, get_set_seq/0, get_seq/0, getall_seq/0]).
-export([clear_set/0, clear_list/0]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {redis_pid, queue, key}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link([]) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Data) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Data], []).




save_seq(Num)->
  gen_server:cast(?SERVER, {save_seq, Num}).


show_seq()->
  gen_server:cast(?SERVER, show_seq).

get_seq() ->
  gen_server:call(?SERVER, get_seq).

getall_seq() ->
  gen_server:call(?SERVER, getall_seq).

save_set(Num)->
  gen_server:cast(?SERVER, {save_set_seq,Num}).

get_set_seq() ->
  gen_server:call(?SERVER, get_set_seq).

clear_set() ->
  gen_server:call(?SERVER, clear_set).
clear_list() ->
  gen_server:call(?SERVER, clear_list).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([{Redis_Host, Redis_Port, Redis_DB, Queue_Key, Set_Key}=Data]) ->
  case eredis:start_link(Redis_Host, Redis_Port, Redis_DB) of
    {ok, Pid} -> io:format("BD Start OK ~n"), {ok, #state{redis_pid = Pid, queue = Queue_Key, key=Set_Key}};
    _ -> io:format("Can't link to REDIS host ~p port ~p DB ~p ~n",[Redis_Host, Redis_Port, Redis_DB]),{ok, #state{}}
  end.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call(clear_list, _From, State) when is_pid(State#state.redis_pid) ->
  eredis:q(State#state.redis_pid, ["DEL", State#state.queue]),
  {reply, ok, State};
handle_call(clear_set, _From, State)  when is_pid(State#state.redis_pid) ->
  eredis:q(State#state.redis_pid, ["DEL", State#state.key]),
  {reply, ok, State};
handle_call(get_set_seq, _From, State)  when is_pid(State#state.redis_pid) ->
  {ok, Numbers} = eredis:q(State#state.redis_pid, ["SMEMBERS", State#state.key]),
  {reply, [binary_to_integer(P) || P <- Numbers], State};
handle_call(get_seq, _From, State)  when is_pid(State#state.redis_pid) ->
  {ok, Numbers} = eredis:q(State#state.redis_pid, ["LRANGE", State#state.queue, 0, -1]),
  {reply, [binary_to_integer(N) || N <- Numbers], State};
handle_call(getall_seq, _From, State)  when is_pid(State#state.redis_pid) ->
  [{ok, Numbers}, _] = eredis:qp(State#state.redis_pid, [["LRANGE", State#state.queue, 0, -1],["DEL", State#state.queue]]),
  {reply, [binary_to_integer(N) || N <- Numbers], State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast({save_set_seq, Num}, State) when is_pid(State#state.redis_pid) ->
  eredis:q(State#state.redis_pid, ["SADD", State#state.key] ++ [integer_to_list(N) || N <- Num]),
  {noreply, State};
handle_cast({save_seq, Num}, State) when is_pid(State#state.redis_pid) ->
%%  eredis:q(State#state.redis_pid, ["LPUSH", State#state.queue] ++ [integer_to_list(N) || N <- Num]),
  eredis:q(State#state.redis_pid, ["RPUSH", State#state.queue] ++ [integer_to_list(N) || N <- Num]),
  {noreply, State};
handle_cast(show_seq, State) when is_pid(State#state.redis_pid)->
  Rez = eredis:q(State#state.redis_pid, ["LRANGE", State#state.queue, 0, -1]),
  io:format("Rez ~p~n", [Rez]),
  {noreply, State};
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
