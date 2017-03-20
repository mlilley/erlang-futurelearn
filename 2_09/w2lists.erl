-module(w2lists).
-export([double/1]).
-include_lib("eunit/include/eunit.hrl").


double([])    -> [];
double([H|T]) -> [2*H | double(T)].

evens([])    -> [];
evens([H|T]) ->
    case H rem 2 == 0 of
        true  -> [H | evens(T)];
        false -> evens(T)
    end.

% --- unit tests ---

double_test() -> 
    ?assert(double([]) == []),
    ?assert(double([1,2,3,4]) == [2,4,6,8]).

evens_test() ->
    ?assert(evens([]) == []),
    ?assert(evens([1,2,3,4]) == [2,4]).

