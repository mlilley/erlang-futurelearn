-module(test).
-export([pieces/1, pieces2/1]).

pieces(0) ->
    1;
pieces(N) ->
    pieces(N-1) + N.

pieces2(N) ->
    (N*N + N + 2) / 2.