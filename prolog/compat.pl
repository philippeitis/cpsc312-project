:- module(compat, [
    prolog_version_eight/0,
    can_use_regex/0
]).
:- dynamic pcre_available/0.

prolog_version_eight :-
    current_prolog_flag(version, Version),
    Version > 80000.

can_use_regex :-
    pcre_available.

can_use_regex :- 
    current_predicate(_, re_match/2),
    assertz(pcre_available).
