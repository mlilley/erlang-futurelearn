-module(rps).
-export([beat/1, lose/1, result/2, tournament/2]).


beat(rock)     -> paper;
beat(paper)    -> scissors;
beat(scissors) -> rock.

lose(rock)     -> scissors;
lose(paper)    -> rock;
lose(scissors) -> paper.

result(rock, paper)     -> -1;
result(rock, scissors)  ->  1;
result(paper, rock)     ->  1;
result(paper, scissors) -> -1;
result(scissors, rock)  -> -1;
result(scissors, paper) ->  1;
result(_X,_X)           ->  0.

tournament(MovesA, MovesB) ->
    lists:sum(
        lists:zipwith(
            fun(MoveA, MoveB) -> result(MoveA, MoveB) end, 
            MovesA, 
            MovesB
        )
    ).