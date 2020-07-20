%%%-------------------------------------------------------------------
%%% @author mva
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2020 6:41
%%%-------------------------------------------------------------------
-module(test_bd).
-author("mva").

-export([all/0]).
-include_lib("eunit/include/eunit.hrl").

setup()->
  Redis_Host = "localhost" ,
  Redis_Port = 6379,
  Redis_DB =2,
  Queue_Key="test1",
  Set_Key="test2",
  {ok, Pid} = bd:start_link({Redis_Host, Redis_Port, Redis_DB, Queue_Key, Set_Key}),
  Pid.

teardown(Pid)->
  bd:clear_set(),
  bd:clear_list(),
  gen_server:stop(Pid).


t(Name, Test) ->
  {Name, {setup, fun setup/0, fun teardown/1, Test}}.


all() ->
  [
    t("save numbers to bd", fun(_) ->
      bd:save_seq([10,20,30,40,50,60]),
      ?_assertEqual([10,20,30,40,50,60], bd:get_seq())
                                    end),

    t("read numbers from bd", fun(_) ->
      bd:save_seq([10, 20, 30, 40, 50, 60]),
      bd:save_seq([1, 2, 3, 4, 5, 6]),
      [
        ?_assertEqual([10, 20, 30, 40, 50, 60, 1, 2, 3, 4, 5, 6], bd:getall_seq()),
        ?_assertEqual([], bd:getall_seq())
      ]
                                       end),

    t("save 'set' to bd", fun(_) ->
      bd:save_set([1,3,4,1]),
      ?_assertEqual([1, 3, 4], bd:get_set_seq())
                                               end),

    t("Clear all", fun(_) ->
      bd:save_set([1,3,4,1]),
      bd:save_seq([10, 20, 30, 40, 50, 60]),
      bd:clear_list(),
      bd:clear_set(),
      ?_assertEqual([],  bd:get_seq()),
      ?_assertEqual([],  bd:get_set_seq())
                                        end)
  ].