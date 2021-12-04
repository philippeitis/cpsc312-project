:- module(sequence_ops, [
    split_left/4,
    levenshtein_distance/3,
    fuzzy_substr/3,
    sequence_match/2,
    join/3,
    list_subset/2
]).

:- use_module(compat, []).
:- if(compat:prolog_version_eight).
:- table(levenshtein_distance/3).
:- table(fuzzy_substr/3).
:- endif.

:- meta_predicate run_levenshtein(+, +, 2, -).

%% list_subset(?List1, ?List2)
% Returns true if List1 is a subset of List2.
list_subset([], _).
list_subset([First|Rest], B) :-
    member(First, B),
    list_subset(Rest, B), !.

%% join(+Items, +Sep, -Output)
% Joins the string with the provided separator string
join([], _Sep, "") :- !.
join([Item], _Sep, Item) :- !.
join([Head|Tail], Sep, Output) :-
    join(Tail, Sep, TailOutput),
    string_concat(Head, Sep, HeadSep),
    string_concat(HeadSep, TailOutput, Output), !.

%% sequence_match/2(+Sequence, +String)
% sequence_match is true if the all elements in Sequence appear in
% String, in sequential order.
sequence_match(Sequence, String) :-
    string(Sequence), string(String),
    string_chars(Sequence, SeqChars),
    string_chars(String, SChars),
    sequence_match(SeqChars, SChars), !.

sequence_match([], _).

sequence_match([Head|Tail], [Head|Tail1]) :-
    sequence_match(Tail, Tail1), !.

sequence_match(Sequence, [_|Tail1]) :-
    sequence_match(Sequence, Tail1).

%% split_left/4(String, Sep, N, -Substrings)
% split_left splits the provided string on the characters in Sep,
% up to a maximum of N times into Substrings. Multiple seperator characters
% will be treated as one.
split_left(String, Sep, N, Substrings) :-
    string_chars(String, Chars),
    string_chars(Sep, Sep_),
    split_left(Chars, Sep_, N, [], CharSubstrings),
    maplist(
        string_chars,
        Substrings,
        CharSubstrings
    ), !.

%% Splits the string from left to right, on the provided separator,
% up to a maximum of n times, and stores intermediate state in Accumulator
split_left([], _Sep, _, Accumulator, [Reversed]) :-
    reverse(Accumulator, Reversed), !.
split_left([Head|Tail], Sep, 0, Accumulator, [Whole]) :-
    member(Head, Sep),
    split_left(Tail, Sep, 0, Accumulator, [Whole]), !.
split_left(String, _Sep, 0, Accumulator, [Whole]) :-
    reverse(Accumulator, Reversed),
    append(Reversed, String, Whole), !.
split_left([Head|Tail], Sep, N, [], Strings) :-
    member(Head, Sep),
    split_left(Tail, Sep, N, [], Strings), !.
split_left([Head|Tail], Sep, N, Accumulator, [Reversed|Strings]) :-
    member(Head, Sep),
    reverse(Accumulator, Reversed),
    % Force early evaluation
    NSub is N - 1,
    split_left(Tail, Sep, NSub, [], Strings), !.
split_left([Head|Tail], Sep, N, Accumulator, Strings) :-
    split_left(Tail, Sep, N, [Head|Accumulator], Strings), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Development notes:
% Haskell impl is ~3x faster (with the backtracking functionality)
% BUT: 
% - +1.2GB of disk usage in Docker
% - Requires reading/writing to streams (annoying)
% - Requires compilation and extra steps in Makefiles

%% Basic implementation of substitution cost.
lev_cost(C, C, 0.0) :- !.
lev_cost(A, B, 0.4) :- downcase_atom(A, C), downcase_atom(B, C), !.
lev_cost(_, _, 1.0) :- !.

%% Fills the current row with the cheapest action
fill_row(_, Row, _, [], Row).
fill_row(PrevRow, Row, Ca, [Cb|StrB], Out) :-
    PrevRow = [Subst,Delete|PRest],
    Row = [Insert|Rest],
    lev_cost(Ca, Cb, SubstCost),
    Substx is Subst + SubstCost,
    Deletex is Delete + 1.0,
    Insertx is Insert + 1.0,
    min_list([Substx, Deletex, Insertx], MinC),
    fill_row([Delete|PRest], [MinC,Insert|Rest], Ca, StrB, Out).

%% Rearranges arguments as needed for initial fill_row call.
% can do [Head|Tail], [HRow, Head|Tail] to keep the full table
% for backtracking purposes
fill_row_helper(StrA, Num, Char, [Head|_], [HRow, Head]) :-
    Numf is float(Num),
    fill_row(Head, [Numf], Char, StrA, RRow),
    reverse(RRow, HRow).

%% Helper which builds the levenshtein distance table (or in this case, a single row).
run_levenshtein(A, "", RowFn, [FirstRow]) :-
    string(A),
    string_chars(A, AChars),
    length(AChars, LA),
    call(RowFn, LA, FirstRow), !.

run_levenshtein(A, B, RowFn, Table) :-
    string(A),
    string(B),
    string_chars(A, AChars),
    string_chars(B, BChars),
    length(AChars, LA),
    length(BChars, LB),
    call(RowFn, LA, FirstRow),
    numlist(1, LB, Nums),
    foldl(fill_row_helper(AChars), Nums, BChars, [FirstRow], Table), !.

to_float(X, Y) :- Y is float(X).

numlist_helper(LA, List) :-
    numlist(0, LA, NList), maplist(to_float, NList, List).

%% levenshtein_distance(+A:str, +B:str, -Distance:float)
% Returns the Levenshtein distance between A and B
% https://en.wikipedia.org/wiki/Levenshtein_distance
% Uses the two-row table solution to optimize for memory and runtime characteristics
levenshtein_distance(A, B, Distance) :-
    run_levenshtein(A, B, numlist_helper, [Row|_]),
    last(Row, Distance).

zeros_helper(LA, List) :-
    LAPlus is LA + 1,
    length(List, LAPlus), maplist(=(0.0), List).

%% levenshtein_distance_fuzzy(+A:str, +B:str, -Distance:float)
% https://en.wikipedia.org/wiki/Approximate_string_matching#Problem_formulation_and_algorithms
% Use min_list for minimum distance. Ignore first item in row.
fuzzy_substr(A, B, Distance) :-
    run_levenshtein(A, B, zeros_helper, [[_|Out]|_]),
    min_list(Out, Distance).

