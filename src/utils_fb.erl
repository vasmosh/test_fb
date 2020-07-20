%%%-------------------------------------------------------------------
%%% @author mva
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jul 2020 15:41
%%%-------------------------------------------------------------------
-module(utils_fb).
-author("mva").

%% API
-export([start_gen/3]).

start_gen(Timer, Pid, Msg)->
  timer:send_interval(Timer, Pid, Msg).