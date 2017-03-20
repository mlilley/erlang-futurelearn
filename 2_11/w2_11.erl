-module(w2_11).
-export([take/2,take2/2]).
-include_lib("eunit/include/eunit.hrl").


%-spec take(integer(),[T]) -> [T].

% direct
take(0,_Z) ->
    [];
take(_N,[]) ->
    [];
take(N,[H|T]) ->
    [H|take(N-1,T)].

% tail
take2(0,_Z) ->
    [];
take2(_N,[]) ->
    [];
take2(N,Z) ->
    take2(N,Z,[]).
take2(_N,[],Out) ->
    Out;
take2(0,_Z,Out) ->
    Out;
take2(N,[H|T],Out) ->
    take2(N-1,T,Out++[H]).


take_test() ->
    ?assert(take(3,[]) == []),
    ?assert(take(3,[1,2]) == [1,2]),
    ?assert(take(3,[1,2,3,4]) == [1,2,3]),

    ?assert(take2(3,[]) == []),
    ?assert(take2(3,[1,2]) == [1,2]),
    ?assert(take2(3,[1,2,3,4]) == [1,2,3]).
