-module(test).
-export([fib/1]).

fib(N) ->
    {P,_} = fibPairs(N),
    P.
fibPairs(0) ->
    {0, 1};
fibPairs(N) ->
    {P, C} = fibPairs(N-1),
    {C, P+C}.

