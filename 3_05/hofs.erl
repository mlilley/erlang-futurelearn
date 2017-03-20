-module(hofs).
-export([doubleAll/1, evens/1, product/1, zip/2, zip_with/3, zip_with2/3, zip2/2]).

doubleAll(Items) ->
    lists:map(fun(I) -> I*2 end, Items).

evens(Items) ->
    lists:filter(fun(I) -> I rem 2 == 0 end, Items).

product([]) ->
    [];
product([X|Xs]) ->
    lists:foldr(fun(I, Prod) -> I * Prod end, X, Xs).

zip([], _) ->
    [];
zip(_, []) ->
    [];
zip([X|Xs], [Y|Ys]) ->
    [{X,Y} | zip(Xs,Ys)].
    
zip_with(_F, [], _) ->
    [];
zip_with(_F, _, []) ->
    [];
zip_with(F, [X|Xs], [Y|Ys]) ->
    [F(X,Y) | zip_with(F, Xs, Ys)].

zip_with2(_F, [], _) ->
    [];
zip_with2(_F, _, []) ->
    [];
zip_with2(F, [X|Xs], [Y|Ys]) ->
    lists:map(fun({Ex,Wy}) -> F(Ex,Wy) end, zip([X|Xs], [Y|Ys])).

zip2([], _) ->
    [];
zip2(_, []) ->
    [];
zip2([X|Xs], [Y|Ys]) ->
    zip_with(fun(Ex,Wy) -> {Ex,Wy} end, [X|Xs], [Y|Ys]).