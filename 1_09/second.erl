-module(second).
-export([hyp/2,perimeter/2,area/2]).

hyp(X,Y) ->
    math:sqrt(first:square(X) + first:square(Y)).

perimeter(X,Y) ->
    hyp(X,Y) + X + Y.

area(X,Y) ->
    first:mult(X,Y) / 2.