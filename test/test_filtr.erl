%%%-------------------------------------------------------------------
%%% @author mva
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2020 7:47
%%%-------------------------------------------------------------------
-module(test_filtr).
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
%%  bd:clear_set(),
%%  bd:clear_list(),
  {ok, Pid_filtr} = filter_seq:start_link(MAX_NUM),
  [Pid_bd, Pid_filtr].

teardown([Pid_bd, Pid_filtr])->
  gen_server:stop(Pid_filtr),
  bd:clear_set(),
  bd:clear_list(),
  gen_server:stop(Pid_bd).


t(Name, Test) ->
  {Name, {setup, fun setup/0, fun teardown/1, Test}}.


all() ->
  [
    t("check filter", fun(_) ->


      bd:save_seq(lists:seq(2, 25)),
      timer:sleep(100),
      Numbers = bd:get_set_seq(),
      ?_assertEqual([2, 3, 5, 7, 11, 13, 17, 19, 23], Numbers)
                   end)

  ].

