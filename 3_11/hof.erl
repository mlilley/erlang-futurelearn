-module(hof).
-export([add/1,times/1,compose/2,id/1,iterate/1,twice/2, multBy3/1]).

add(X) ->
    fun(Y) -> X+Y end.

times(X) ->
    fun(Y) ->
	     X*Y end.

compose(F,G) ->
    fun(X) -> G(F(X)) end.

id(X) ->
    X.

iterate(0) ->
    dummy;
iterate(N) ->
    dummy.
      

multBy3(X) ->
    X*3.

twice(F,X) ->
    F(F(X)).


% ?
iterate(0) ->
    fun(F) ->
        F();

iterate(N) ->
    fun(F) ->
        compose(F, (iterate(N-1))(F)) end.
        
