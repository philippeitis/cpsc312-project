:- module(string_op, [split_left/4, levenshtein_distance/3]).
:- table levenshtein_distance/3.

% split_left/4 splits the provided string on the characters in Sep,
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

% [Char], [Sep], N, [Acc], [Reversed]
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

%%levenshtein_distance(A, B, Distance) :-
levenshtein_distance(A, B, Distance) :-
    string(A),
    string(B),
    string_chars(A, AChars),
    string_chars(B, BChars),
    levenshtein_distance(AChars, BChars, Distance), !.
levenshtein_distance([], B, Distance) :- length(B, Distance), !.
levenshtein_distance(A, [], Distance) :- length(A, Distance), !.
levenshtein_distance([X|TailA], [X|TailB], Distance) :-
    levenshtein_distance(TailA, TailB, Distance), !.
levenshtein_distance([A|TailA], [B|TailB], Distance) :-
    levenshtein_distance(TailA, [B|TailB], Distance1),
    levenshtein_distance([A|TailA], TailB, Distance2),
    levenshtein_distance(TailA, TailB, Distance3),
    min_list([Distance1, Distance2, Distance3], DistanceMin),
    Distance is DistanceMin + 1, !.