-module(w2lists).
-export([product/1]).
-include_lib("eunit/include/eunit.hrl").


% accepts empty list
product(List) ->
    product(List,1).
product([], P) -> 
    P;
product([H|T], P) -> 
    product(T, P*H).

% does not accept empty list
maximum([H|T]) ->
    maximum([H|T], H).
maximum([], M) ->
    M;
maximum([H|T], M) ->
    maximum(T, max(H,M)).

maximum2([H]) ->
    H;
maximum2([H|T]) ->
    max(H, maximum2(T)).


% --- unit tests ---

product_test() -> 
    ?assert(product([]) == 1),
    ?assert(product([1,2,3,4]) == 24).

maximum_test() ->
    ?assert(maximum([1]) == 1),
    ?assert(maximum([3,2,5]) == 5),
    ?assert(maximum([1,2,-3]) == 2).
