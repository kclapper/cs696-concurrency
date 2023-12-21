
-module(test).

-export([start/0]).


xs_win() ->
    tic_tac_toe:join_game(),
    tic_tac_toe:join_game(),
    tic_tac_toe:make_move(1),
    tic_tac_toe:make_move(4),
    tic_tac_toe:make_move(2),
    tic_tac_toe:make_move(5),
    {win, xs, _} = tic_tac_toe:make_move(3),
    {xs_win, passed}.

start() ->
    tic_tac_toe:start_link(),
    xs_win().
