-module(shapes).
-export([perimeter/1, area/1, enclose/1]).
-include_lib("eunit/include/eunit.hrl").

% Michael Lilley
% 2017-03-12
%
% Valid shapes:
%  {circle,{X,Y},R}
%  {rectangle,{X,Y},H,W} 
%  {triangle,{X,Y},A,B,Theta}  (side lengths A, B, & angle b/w of Theta)
%
% Assumes: "smallest enclosing rectangle" means rectangle w/ smallest area.
%
% Min Area Rectangle will have one side co-incident with one side of the 
% triangle thus, we have 3 cases to compute a bounding rectangle for, then 
% find the one with minimum area.
%
% To find each, it converts the triangle to a list of points, then for each
% side, rotates the points such that the side is aligned to the +ve x-axis,
% and computes the min bounding rectangle for the points in this orientation.
% With these 3 rectangles, we choose the one with the minimum area.
%
% To run unit tests:
%   > c(shapes).
%   > shapes:test().

% Calculate perimeter of various shapes
perimeter({circle,{_X,_Y},R}) ->
    2 * math:pi() * R;
perimeter({rectangle,{_X,_Y},H,W}) ->
    2 * (H + W);
perimeter({triangle,{_X,_Y},A,B,Theta}) ->
    case Theta > math:pi() of
        true  -> throw('arg Theta must be <= PI');
        false -> math:sqrt(A*A + B*B - 2*A*B*math:cos(Theta)) + A + B
    end.

% Calculate area of various shapes
area({circle,{_X,_Y},R}) ->
    math:pi() * R * R;
area({rectangle,{_X,_Y},H,W}) ->
    H * W;
area({triangle,{_X,_Y},A,B,Theta}) ->
    case Theta > math:pi() of
        true  -> throw('arg Theta must be <= PI');
        false -> A * B * math:sin(Theta) / 2
    end.

% Calculate min bounding rectangle of various shapes
enclose({circle,{X,Y},R}) ->
    {rectangle,{X,Y},2*R,2*R};
enclose({rectangle,{X,Y},H,W}) ->
    {rectangle,{X,Y},H,W};
enclose({triangle,{X,Y},A,B,Theta}) ->
    case Theta > math:pi() of
        true  -> throw('arg Theta must be <= PI');
        false -> minItem(boundingRects({triangle,{X,Y},A,B,Theta}), 
            fun area/1)
    end.


% --- helpers ----------------------------

% Finds all bounding rectangles for a triangle, where each is coincident with
% a different side of the provided triangle.
boundingRects({triangle,{X,Y},A,B,Theta}) ->
    {points,{_Ex,_Wy},Coords} = toPoints({triangle,{X,Y},A,B,Theta}),
    lists:map(fun(N) -> 
        boundingRect(alignToXAxis({points,{X,Y},rotateList(Coords,N)}))
            end, lists:seq(0,2)).

% Find the bounding rectangle that encompases a set of points.
boundingRect({points,{X,Y},Coords}) ->
    {rectangle,{X,Y},
        abs(maxY(Coords)-minY(Coords)),abs(maxX(Coords)-minX(Coords))}.

% Converts a triangle its set of points (centered about 0,0).
% Returns {points,{X,Y},[{X1,Y1},...]}
toPoints({triangle,{X,Y},A,B,Theta}) ->
    % (assume for the moment that point of intersection is 0,0)
    Coords = [{0,0},{A,0},{B*math:cos(Theta),B*math:sin(Theta)}],
    % find centroid and center points around it
    {Cx,Cy} = centroid(Coords),
    translate2D({points,{Cx+X,Cy+Y},Coords}, -Cx, -Cy).

% Rotates all points such that the side formed by the first two points is
% aligned with the +ve x-axis.
% Assumes Coords are situated about origin, so no translations necessary.
alignToXAxis({points,{X,Y},Coords}) ->
    [{X1,Y1}|[{X2,Y2}|_]] = Coords,
    Angle = math:atan((Y2-Y1)/(X2-X1)),
    rotate2D({points,{X,Y},Coords}, -Angle).

% Finds the centroid of a list of coordinates
centroid(Coords) ->
    N = length(Coords),
    SumX = lists:foldl(fun({X,_Y},Acc) -> Acc + X end, 0, Coords),
    SumY = lists:foldl(fun({_X,Y},Acc) -> Acc + Y end, 0, Coords),
    {SumX/N,SumY/N}.

% 2D Translation
translate2D({points,{X,Y},Coords}, Tx, Ty) ->
    {points,{X+Tx,Y+Ty},lists:map(fun({Ex,Wy}) -> {Ex+Tx,Wy+Ty} end, Coords)}.

% 2D Rotation (about origin)
rotate2D({points,{X,Y},Coords}, Angle) ->
    {points,{X,Y},lists:map(fun({Ex,Wy}) -> 
        {Ex*math:cos(Angle) - Wy*math:sin(Angle),
         Wy*math:cos(Angle) + Ex*math:sin(Angle)} end, Coords)}.

% Finds the min/max X/Y ordinate of a list of coordinates
minX(Coords) -> lists:min(lists:map(fun({X,_Y}) -> X end, Coords)).
maxX(Coords) -> lists:max(lists:map(fun({X,_Y}) -> X end, Coords)).
minY(Coords) -> lists:min(lists:map(fun({_X,Y}) -> Y end, Coords)).
maxY(Coords) -> lists:max(lists:map(fun({_X,Y}) -> Y end, Coords)).

% Rotates a list N items to the left
rotateList([], _N)         -> [];
rotateList([H1|[]], _N)    -> [H1];
rotateList([H1|[H2|T]], 0) -> [H1|[H2|T]];
rotateList([H1|[H2|T]], N) -> rotateList([H2|T] ++ [H1], N-1).

% Finds item in List for which ToVal function returns a minimum.
% List must be non-empty.
minItem(List,ToVal) ->
    lists:foldl(fun(Item,Acc) -> case ToVal(Item) < ToVal(Acc) of 
        true -> Item; 
        false -> Acc 
    end end, hd(List), List).



% --- unit tests ---------------------------

approxEq(A, B, Precision) ->
    (B >= A - Precision) and (B =< A + Precision).

minItem_test() ->
    List  = [{2}, {3}, {1}, {4}, {5}],
    ToVal = fun({V}) -> V end,
    Min   = minItem(List, ToVal),
    ?assert(Min == {1}).

rotateList1_test() -> ?assert(rotateList([], 0) == []).
rotateList2_test() -> ?assert(rotateList([], 1) == []).
rotateList3_test() -> ?assert(rotateList([], 2) == []).
rotateList4_test() -> ?assert(rotateList([1], 0) == [1]).
rotateList5_test() -> ?assert(rotateList([1], 1) == [1]).
rotateList6_test() -> ?assert(rotateList([1], 2) == [1]).
rotateList7_test() -> ?assert(rotateList([1,2], 0) == [1,2]).
rotateList8_test() -> ?assert(rotateList([1,2], 1) == [2,1]).
rotateList9_test() -> ?assert(rotateList([1,2], 2) == [1,2]).
rotateList10_test() -> ?assert(rotateList([1,2,3], 0) == [1,2,3]).
rotateList11_test() -> ?assert(rotateList([1,2,3], 1) == [2,3,1]).
rotateList12_test() -> ?assert(rotateList([1,2,3], 2) == [3,1,2]).
rotateList13_test() -> ?assert(rotateList([1,2,3], 3) == [1,2,3]).

minX_test() -> ?assert(minX([{1,2},{3,4},{-2,8}]) == -2).
minY_test() -> ?assert(minY([{1,2},{3,4},{-2,8}]) == 2).
maxX_test() -> ?assert(maxX([{1,2},{3,4},{-2,8}]) == 3).
maxY_test() -> ?assert(maxY([{1,2},{3,4},{-2,8}]) == 8).

rotate2D_test() -> 
    {points,{1,2},[{X1,Y1}|[{X2,Y2}|[{X3,Y3}|[{X4,Y4}]]]]} = 
        rotate2D({points,{1,2},[{1,0},{0,2},{-3,0},{0,-4}]}, math:pi()/2),
    ?assert(approxEq(X1,  0, 0.0001)),
    ?assert(approxEq(Y1,  1, 0.0001)),
    ?assert(approxEq(X2, -2, 0.0001)),
    ?assert(approxEq(Y2,  0, 0.0001)),
    ?assert(approxEq(X3,  0, 0.0001)),
    ?assert(approxEq(Y3, -3, 0.0001)),
    ?assert(approxEq(X4,  4, 0.0001)),
    ?assert(approxEq(Y4,  0, 0.0001)).

translate2D_test() ->
    {points,{X,Y},[{X1,Y1}]} =
        translate2D({points,{-10,10},[{-1,3}]}, 10, -5),
    ?assert(approxEq(X,   0, 0.0001)),
    ?assert(approxEq(Y,   5, 0.0001)),
    ?assert(approxEq(X1,  9, 0.0001)),
    ?assert(approxEq(Y1, -2, 0.0001)).

centroid_test() ->
    {X,Y} = centroid([{0,0}, {2,1}, {5,2}]),
    ?assert(approxEq(X, 2.333333, 0.0001)),
    ?assert(approxEq(Y, 1, 0.0001)).

alignToXAxis_test() ->
    {points,{X,Y},[{X1,Y1},{X2,Y2},{X3,Y3}]} = 
        alignToXAxis({points,{10,5},[{1,1},{3,2},{-5,-3}]}),
    ?assert(approxEq(X,  10, 0.0001)),
    ?assert(approxEq(Y,   5, 0.0001)),
    ?assert(approxEq(X1,  1.341640, 0.0001)),
    ?assert(approxEq(Y1,  0.447214, 0.0001)),
    ?assert(approxEq(X2,  3.577708, 0.0001)),
    ?assert(approxEq(Y2,  0.447214, 0.0001)),
    ?assert(approxEq(X3, -5.813776, 0.0001)),
    ?assert(approxEq(Y3, -0.447214, 0.0001)).

toPoints_test() ->
    {points,{X,Y},[{X1,Y1},{X2,Y2},{X3,Y3}]} = 
        toPoints({triangle,{10,5},5,20,0.175}),
    ?assert(approxEq(X,  10, 0.0001)),
    ?assert(approxEq(Y,   5, 0.0001)),
    ?assert(approxEq(X1, -8.231510, 0.0001)),
    ?assert(approxEq(Y1, -1.160721, 0.0001)),
    ?assert(approxEq(X2, -3.231510, 0.0001)),
    ?assert(approxEq(Y2, -1.160721, 0.0001)),
    ?assert(approxEq(X3, 11.463021, 0.0001)),
    ?assert(approxEq(Y3,  2.321442, 0.0001)).

boundingRect_test() ->
    {rectangle,{X,Y},H,W} = 
        boundingRect({points,{5,10},[{-3,1},{1,-4},{0,2}]}),
    ?assert(approxEq(X, 5, 0.0001)),
    ?assert(approxEq(Y, 10, 0.0001)),
    ?assert(approxEq(H, 6, 0.0001)),
    ?assert(approxEq(W, 4, 0.0001)).

boundingRects_test() ->
    Rects = boundingRects({triangle,{1,2},2,3,3*math:pi()/4}),
    ?assert(length(Rects) == 3).

perimeter_circle1_test() -> 
    ?assert(approxEq(perimeter({circle,{0,0},1}), 6.283185, 0.0001)).
perimeter_circle2_test() ->
    ?assert(approxEq(perimeter({circle,{0,0},0.5}), 3.141592, 0.0001)).
perimeter_rectangle1_test() ->
    ?assert(approxEq(perimeter({rectangle,{0,0},1,2}), 6, 0.0001)).
perimeter_rectangle2_test() ->
    ?assert(approxEq(perimeter({rectangle,{0,0},10,20}), 60, 0.0001)).
perimeter_triangle1_test() ->
    ?assert(approxEq(perimeter({triangle,{0,0},3,4,1.570796}), 12, 0.0001)).
perimeter_triangle2_test() ->
    ?assert(approxEq(perimeter({triangle,{0,0},3,4,2.356194}), 13.478469, 
        0.0001)).

area_circle1_test() ->
    ?assert(approxEq(area({circle,{0,0},1}), 3.141592, 0.0001)).
area_circle2_test() ->
    ?assert(approxEq(area({circle,{0,0},2}), 12.566370, 0.0001)).
area_rectangle1_test() ->
    ?assert(approxEq(area({rectangle,{0,0},1,2}), 2, 0.0001)).
area_rectangle2_test() ->
    ?assert(approxEq(area({rectangle,{0,0},3.5,2.8}), 9.8, 0.0001)).
area_triangle1_test() -> 
    ?assert(approxEq(area({triangle,{0,0},3,4,1.570796}), 6, 0.0001)).
area_triangle2_test() -> 
    ?assert(approxEq(area({triangle,{0,0},3,4,2.356194}), 4.242640, 0.001)).

enclose_circle1_test() ->
    ?assert(enclose({circle,{0,0},1}) == {rectangle,{0,0},2,2}).
enclose_circle2_test() ->
    ?assert(enclose({circle,{0,0},0.5}) == {rectangle,{0,0},1,1}).
enclose_rectangle1_test() ->
    ?assert(enclose({rectangle,{0,0},1,2}) == {rectangle,{0,0},1,2}).
enclose_rectangle2_test() ->
    ?assert(enclose({rectangle,{2,6},2,4}) == {rectangle,{2,6},2,4}).
enclose1_triangle_test() ->
    {rectangle,{X,Y},H,W} = enclose({triangle,{0,0},3,2,3*math:pi()/4}),
    ?assert(approxEq(X, 0, 0.0001)),
    ?assert(approxEq(Y, 0, 0.0001)),
    ?assert(approxEq(H, 0.915304, 0.0001)),
    ?assert(approxEq(W, 4.635221, 0.0001)).
