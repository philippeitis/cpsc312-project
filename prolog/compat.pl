:- module(compat, [prolog_version_eight/0]).

prolog_version_eight :-
    current_prolog_flag(version, Version),
    Version > 80000.