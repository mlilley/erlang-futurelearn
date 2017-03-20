-module(index).
-export([index/1,index/2]).
-include_lib("eunit/include/eunit.hrl").

% Michael Lilley
% 2017-03-18
% 
% To run:
%   $ erl
%   > c(index).
%   > index("gettysburg-address.txt").      # to index words of any length
%   > index("gettysburg-address.txt", 4).   # to index only words >= 4 chars
%
% To run unit tests:
%   $ erl
%   > c(index).
%   > index:test().
%
% Solution solves the base assignment problem, plus the following further problems:
%  - removing short words (index/2)
%  - sorting output so words appear in lexicographic order
%  - normalizing word case, so that "foo" and "Foo" are considered the same word
%
% Additionally supplies a bugfixed version of the provided get_all_lines/2 function.
%  (see below).


% Index contents of Filename, producing a list of {Word,IndexRanges} tuples
index(Filename) ->
    rangify_words(condense_words(sort_words(flatten_words(map_with_index(
        fun(Line, LineNum) -> to_words(remove_punct(lower_case(Line)), LineNum) end, 
        get_file_contents(Filename)
    ))))).

% Index contents of Filename, producing a list of {Word,IndexRanges} tuples,
% excluding words that are shorter than MinLength
index(Filename, MinLength) ->
    rangify_words(condense_words(sort_words(flatten_words(map_with_index(
        fun(Line, LineNum) -> to_words(remove_punct(lower_case(Line)), LineNum, MinLength) end, 
        get_file_contents(Filename)
    ))))).


% --- helpers ---

% Applies rangify() to the Line number indexes array in each {Word,Indexes} tuple.
rangify_words([]) ->
    [];
rangify_words([{_Word,[_Line|_Lines]}|_Words]=Words) ->
    lists:map(fun({Word,Lines}) -> {Word,rangify(Lines)} end, Words).
    
% Converts a list of sorted {Word,Line} tuples to [{Word,Lines}]
% ex: [{"cat",1},{"cat",1},{"cat",3}] -> [{"cat",[1,1,3]}]
condense_words([]) ->
    [];
condense_words([{Word,Line}|T]) ->
    condense_words(T,Word,[Line],[]).
condense_words([],PrevWord,PrevLines,Acc) ->
    Acc ++ [{PrevWord,PrevLines}];
condense_words([{Word,Line}|Words],PrevWord,PrevLines,Acc) ->
    case Word == PrevWord of 
        true  -> condense_words(Words,Word,PrevLines ++ [Line],Acc);
        false -> condense_words(Words,Word,[Line],Acc ++ [{PrevWord,PrevLines}])
    end.

% Sort a list of {Word,Line} tuples by Word, then by Line
sort_words([]) ->
    [];
sort_words([{_Word,_Line}|_T]=W) ->
    lists:sort(W).

% Converts [[{"cat",1},{"cat",2}],[{"cat",3}]] to [{"cat",1},{"cat",2},{"cat",3}]
flatten_words(Words) ->
    lists:append(Words).

% Splits a string into {Word,Line} tuples
to_words(Line, LineNum) ->
    lists:map(fun(Word) -> {Word,LineNum} end, string:tokens(Line, " ")).

% Splits a string into {Word,Line} tuples, removing words that are shorter than MinLength
to_words(Line, LineNum, MinLength) ->
    lists:map(
        fun(Word) -> {Word,LineNum} end, 
        lists:filter(
            fun(X) -> length(X) >= MinLength end,
            string:tokens(Line, " ")
        )
    ).

% Strips punctuation from a string.
% NB: Doesn't bother trying to deal with cases like "Doesn't" or "moo-\ncows"
remove_punct([]) ->
    [];
remove_punct([X|Xs]) ->
    case lists:member(X, ".,;:-?!<>()[]{}+=*&^%$#@/\\`\'\"\t\n") of
        true  -> remove_punct(Xs);
        false -> [ X | remove_punct(Xs) ]
    end.

% Converts a string to lower case.
lower_case([]) ->
    [];
lower_case([X|Xs]) ->
    [lower_case(X) | lower_case(Xs)];
lower_case(X) ->
    case $A =< X andalso X =< $Z of
        true  -> X + 32;
        false -> X
    end.

% Same as lists:map but also passes index of item to Fn
map_with_index(_Fn, []) ->
    [];
map_with_index(Fn, List) ->
    map_with_index(Fn, List, 1, []).
map_with_index(_Fn, [], _Index, Result) ->
    Result;
map_with_index(Fn, List, Index, Result) ->
    map_with_index(Fn, tl(List), Index+1, Result ++ [Fn(hd(List), Index)]).

% Converts a list of integers into a list of sorted tuple ranges
% ex: [10,5,7,1,6,9,9,9] -> [{1,1},{5,7},{9,10}]
rangify([]) ->
    [];
rangify(Lines) -> 
    SortedLines = lists:sort(Lines),
    rangify(tl(SortedLines),{hd(SortedLines),hd(SortedLines)},[]).
rangify([],{L1,L2},Ranges) ->
    Ranges ++ [{L1,L2}];
rangify([Line|Lines],{L1,L2},Ranges) ->
    if
        Line == L2     -> rangify(Lines,{L1,L2},Ranges);
        Line == L2 + 1 -> rangify(Lines,{L1,Line},Ranges);
        true           -> rangify(Lines,{Line,Line},Ranges ++ [{L1,L2}])
    end.


% --- supplied code ----

% Get the contents of a text file into a list of lines.
% Each line has its trailing newline removed.

get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
lists:reverse(Rev).

% Auxiliary function for get_file_contents.
% Not exported.

get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line ->
            % BUGFIX: provided get_all_lines/2 stripped last character in file
            %   if last char was not a newline.
            {Strip,_} = case [hd(lists:reverse(Line))] == "\n" of
                true  -> lists:split(length(Line)-1,Line);
                false -> lists:split(length(Line),Line)
            end,
            get_all_lines(File,[Strip|Partial])
    end.


% --- unit tests ---

get_all_lines_test() ->
    % test bugfix
    ?assert(get_file_contents("test1.txt") == ["Foo bar","bar baz"]).

index_test() ->
    ?assert(index("test1.txt") == [{"bar",[{1,2}]},{"baz",[{2,2}]},{"foo",[{1,1}]}]).

index_minlength_test() ->
    ?assert(index("test2.txt",4) == [{"brown",[{1,2}]},{"moocows",[{1,1}]}]).

condense_words_test() ->
    ?assert(condense_words([]) == []),
    ?assert(condense_words([{"cat",1},{"cat",1},{"cat",3}]) == [{"cat",[1,1,3]}]).

sort_words_test() ->
    ?assert(sort_words([]) == []),
    ?assert(sort_words([{"foo",2},{"foo",1},{"bar",7},{"baz",3}]) == [{"bar",7},{"baz",3},{"foo",1},{"foo",2}]).

flatten_words_test() ->
    ?assert(flatten_words([[]]) == []),
    ?assert(flatten_words([[],[]]) == []),
    ?assert(flatten_words([[{"cat",1},{"cat",2}],[{"cat",3}]]) == [{"cat",1},{"cat",2},{"cat",3}]).

to_words_test() ->
    ?assert(to_words("", 5) == []),
    ?assert(to_words("foo bar baz", 5) == [{"foo",5},{"bar",5},{"baz",5}]).

remove_punct_test() ->
    ?assert(remove_punct("") == ""),
    ?assert(remove_punct("Hey, how about Heidi's headlights?") == "Hey how about Heidis headlights").

lower_case_test() ->
    ?assert(lower_case("") == ""),
    ?assert(lower_case("Foo") == "foo"),
    ?assert(lower_case("bar") == "bar"),
    ?assert(lower_case("BAZ!") == "baz!").

map_with_index_test() ->
    ?assert(map_with_index(fun(X,I) -> {X,I} end, []) == []),
    ?assert(map_with_index(fun(X,I) -> {X,I} end, [a,b]) == [{a,1},{b,2}]).

rangify_test() ->
    ?assert(rangify([]) == []),
    ?assert(rangify([10,5,7,1,6,9,9,9]) == [{1,1},{5,7},{9,10}]).