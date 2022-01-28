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

:- use_foreign_library('./bin/libutils_rs.so').

:- meta_predicate run_levenshtein(+, +, 2, -).

%! list_subset(?List1, ?List2)
% Returns true if List1 is a subset of List2.
list_subset([], _).
list_subset([First|Rest], B) :-
    member(First, B),
    list_subset(Rest, B), !.

%! join(+Items, +Sep, -Output)
% Joins the string with the provided separator string
join([], _Sep, "") :- !.
join([Item], _Sep, Item) :- !.
join([Head|Tail], Sep, Output) :-
    join(Tail, Sep, TailOutput),
    string_concat(Head, Sep, HeadSep),
    string_concat(HeadSep, TailOutput, Output), !.

%! sequence_match/2(+Sequence, +String)
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Documentation for foreign library
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%! levenshtein_distance(+A:str, +B:str, -Distance:float) is semidet
% Returns the Levenshtein distance between A and B
% https://en.wikipedia.org/wiki/Levenshtein_distance

%! levenshtein_distance_fuzzy(+A:str, +B:str, -Distance:float) is semidet
% https://en.wikipedia.org/wiki/Approximate_string_matching#Problem_formulation_and_algorithms

%! split_left/4(String, Sep, N, -Substrings)
% split_left splits the provided string on the characters in Sep,
% up to a maximum of N times into Substrings. Multiple seperator characters
% will be treated as one.
