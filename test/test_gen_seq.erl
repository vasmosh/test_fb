%%%-------------------------------------------------------------------
%%% @author mva
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2020 7:13
%%%-------------------------------------------------------------------
-module(test_gen_seq).
-author("mva").
-export([all/0]).
-include_lib("eunit/include/eunit.hrl").

setup()->
  Redis_Host = "localhost" ,
  Redis_Port = 6379,
  Redis_DB =2,
  Queue_Key="test1",
  Set_Key="test2",
  MAX_NUM = 25,
  {ok, Pid_bd} = bd:start_link({Redis_Host, Redis_Port, Redis_DB, Queue_Key, Set_Key}),
  bd:clear_set(),
  bd:clear_list(),
  {ok, Pid_gen} = gen_seq:start_link(MAX_NUM),
  [Pid_bd, Pid_gen].

teardown([Pid_bd, Pid_gen])->
  gen_server:stop(Pid_gen),
  bd:clear_set(),
  bd:clear_list(),
  gen_server:stop(Pid_bd).


t(Name, Test) ->
  {Name, {setup, fun setup/0, fun teardown/1, Test}}.


all() ->
  [
    t("check gen", fun(_) ->
      timer:sleep(1000),
      Numbers = bd:getall_seq(),
      ?_assertEqual(ordsets:from_list(Numbers), ordsets:from_list(lists:seq(2, 25))),
      ?_assert((2970 =< length(Numbers)) and (length(Numbers) =< 3030))
                                                       end)

  ].
