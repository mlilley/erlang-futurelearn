-module(bits).
-export([bits/1]).
-include_lib("eunit/include/eunit.hrl").

% Michael Lilley
% 2017-03-12
%
% Sums the number of 'on' bits in a given positive integer.
%
% Tail-recursive vs direct
% ------------------------
% Tail-recursive implementations have the advantages:
% - internally can be converted to a loop, instead of nested function calls,
%   which means we wont exhaust the stack (unlike direct) in tail-call
%   optimized programming languages.
% - being a loop, may execute faster due to not having to create new stack
%   frames for each cycle.
% - often use less memory.
%
% Whereas direct-recursive implementations are:
% - often more intuitive / more clearly illustrate the work being done.
%
% Because direct-recursive implementations will eventually exhaust the stack
% and crash if they execute too many cycles, tail-recursive implementations 
% are generally preferred.  For these reasons, I prefer the bits2/1
% implementation below.


% (direct-recursive)
bits(N) ->
    case N == 0 of
        true -> 0;
        false -> 1 + bits(N band (N-1))
    end.

% --- PREFERRED ---
% (tail-recursive)
bits2(N) ->
    bits2(N,0).
bits2(0,Acc) ->
    Acc;
bits2(N,Acc) ->
    bits2(N band (N-1), Acc + 1).
    
% (tail-recursive, alternative)
% Uses a bit mask to test each bit in turn starting from 1, until
% the mask is > N.
bits3(N) -> 
    bits3(N,1,0).
bits3(N,M,S) when M > N -> 
    S;
bits3(N,M,S) -> 
    bits3(N,M bsl 1, S + case N band M == 0 of
        true  -> 0;
        false -> 1
    end).


% --- unit tests ------------------------

bits_test() -> 
    ?assert(bits(7) == 3),
    ?assert(bits(8) == 1),
    ?assert(bits(27) == 4),
    ?assert(bits(98) == 3),
    ?assert(bits(219) == 6).

bits2_test() ->
    ?assert(bits2(7) == 3),
    ?assert(bits2(8) == 1),
    ?assert(bits2(27) == 4),
    ?assert(bits2(98) == 3),
    ?assert(bits(219) == 6).

bits3_test() ->
    ?assert(bits3(7) == 3),
    ?assert(bits3(8) == 1),
    ?assert(bits3(27) == 4),
    ?assert(bits3(98) == 3),
    ?assert(bits(219) == 6).
