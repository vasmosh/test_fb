%%%-------------------------------------------------------------------
%%% @author mva
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 20. Jul 2020 6:53
%%%-------------------------------------------------------------------
-module(run_tests).
-author("mva").

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  {inorder, lists:flatten([
    test_bd:all(),
    test_gen_seq:all(),
    test_filtr:all()
  ])}.

