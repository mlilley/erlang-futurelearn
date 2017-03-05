-module(test).
-export([fib/1,perfect/1]).


fib(N) ->
    fib(N,1).

fib(0,A) ->
    A;
fib(N,A) ->
    fib(N-1,A*N).


perfect(N) ->
    N == sum_divisors(N,N div 2,0).

sum_divisors(N,1,Sum) ->
    Sum;
sum_divisors(N,Divisor,Sum) ->
    sum_divisors(N,Divisor)
