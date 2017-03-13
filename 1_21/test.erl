-module(test).
-export([fac/1,fac2/1,loop/1,sumTo/1,fib/1,fib2/1,perfect/1]).

% factorial (direct recursive)
fac(0) ->
    1;
fac(N) ->
    fac(N-1)*N.

% factorial (tail recursive)
fac2(N) ->
    fac2(N,1).
fac2(0,P) ->
    P;
fac2(N,P) ->
    fac2(N-1, P*N).

% loop
loop(N) when N>0 ->
    io:format("~p~n",[N]),
    loop(N-1);
loop(_N) ->
    io:format("bye~n").

% Exercises

% Sub to F(0)...F(N)
sumTo(N) -> 
    sumTo(N, 0).
sumTo(0, Acc) ->
    Acc;
sumTo(N, Acc) ->
    sumTo(N-1, Acc+N).

% Max F(0)...F(N) ?

% fib (direct)
fib(0) -> 0;
fib(1) -> 1;
fib(N) -> fib(N-2) + fib(N-1).

% fib (tail recursive)
fib2(N) -> 
    fib2(0,1,N).
fib2(A,_B,0) -> 
    A;
fib2(A,B,N) ->
    fib2(B, B+A, N-1).
    
% perfect (sum of N's divisors == N)
perfect(N) ->
    perfect(N,N div 2,1).
perfect(N,X,A) when (X-1 < 1) ->    
    A == N;
perfect(N,X,A) when (N rem X == 0) ->
    perfect(N,X-1,A+X);
perfect(N,X,A) ->
    perfect(N,X-1,A).
    