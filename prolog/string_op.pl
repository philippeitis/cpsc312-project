:- module(string_op, [split_left/4]).

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