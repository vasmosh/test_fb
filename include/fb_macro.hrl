%%%-------------------------------------------------------------------
%%% @author mva
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. Jul 2020 14:23
%%%-------------------------------------------------------------------
-author("mva").

-define(APPLICATION, test_fb).

-define(RATE, 3000).

%%RATE = (1000ms /TIMESTEP )*NUMSTEP
-define(TIMESTEP, 10). %ms
-define(NUMSTEP, 30). %num

-define(TIMEFILTER, 10). %ms