-module(test).
-export([xOr1/2,xOr2/2,xOr3/2,xOr4/2,xOr5/2,xOr6/2,maxThree/3,howManyEqual/3]).

% a  b  a xor b
% -------------
% T  T     F
% T  F     T
% F  T     T
% F  F     F     

xOr1(true,false) ->
    true;
xOr1(false,true) ->
    true;
xOr1(_,_) ->
    false.

xOr2(X,X) ->
    false;
xOr2(_,_) ->
    true.

xOr3(X,Y) ->
    X =/= Y.

xOr4(X,Y) ->
    not (X == Y).

xOr5(X,Y) ->
    (((X == true) and (Y == false)) or ((X == false) and (Y == true))).

xOr6(true,X) ->
    not(X);
xOr6(false,X) ->
    X.

maxThree(X,Y,Z) ->
    max(max(X,Y),Z).

howManyEqual(_X,_X,_X) ->
    3;
howManyEqual(_X,_X,_Y) ->
    2;
howManyEqual(_X,_Y,_X) ->
    2;
howManyEqual(_Y,_X,_X) ->
    2;
howManyEqual(_X,_Y,_Z) ->
    0.

