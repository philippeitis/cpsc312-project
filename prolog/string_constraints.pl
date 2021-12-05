:- module(string_constraints, [
    equality_constraint/5,
    substring_constraint/5,
    levenshtein_constraint/6,
    subsequence_constraint/5,
    regex_constraint/5,
    similarity_constraint/5,
    sub_similarity_constraint/5,
    fuzzy_substr_constraint/5,
    add_string_constraint/5
]).

:- use_module(compat, []).
:- use_module(constraints, [no_constraint/3, and_constraint/5]).
:- use_module(sequence_ops).
:- use_module(nlp).

:- meta_predicate wrap_core(2, 2, ?, -, -).
:- meta_predicate equality_core(2, +, ?, -, -).
:- meta_predicate substring_constraint(2, +, ?, -, -).
:- meta_predicate levenshtein_constraint(2, +, +, +, -, -).
:- meta_predicate subsequence_constraint(2, +, +, -, -).
:- meta_predicate regex_constraint(2, +, ?, -, -).
:- meta_predicate similarity_constraint(2, +, ?, -, -).
:- meta_predicate sub_similarity_constraint(2, +, ?, -, -).
:- meta_predicate fuzzy_substr_constraint(2, +, ?, -, -).
:- meta_predicate add_string_constraint(2, +, ?, +, -).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Wrappers for string comparison methods

wrapper(Constraint, Getter, Item, Cost, NewConstraint) :-
    wrap_core(Constraint, Getter, Item, Cost, NewConstraint).

wrap_core(Constraint, Getter, Item, Cost, no_constraint) :-
    call(Getter, Item, Value),
    call(Constraint, Value, Cost).

wrap_core(Constraint, Getter, Item, 1.0, wrapper(Constraint, Getter)) :-
    call(Getter, Item, Value),
    \+call(Constraint, Value, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% String comparison methods
% x_core: Args*, Value, Cost
% x_constraint: Args*, Func, Cost, NewConstraint

equality_core(Target, Target, 0.0).
equality_constraint(Getter, Target, Func, Cost, NewConstraint) :-
    wrap_core(equality_core(Target), Getter, Func, Cost, NewConstraint).

sub_core(Needle, String, 0.0) :-
    sub_string(String, _, _, _, Needle).
substring_constraint(Getter, Needle, Func, Cost, NewConstraint) :-
    wrap_core(sub_core(Needle), Getter, Func, Cost, NewConstraint).

lev_core(Target, MaxDistance, String, Distance) :-
    levenshtein_distance(Target, String, LevDistance),
    LevDistance =< MaxDistance,
    Distance is LevDistance / MaxDistance.
levenshtein_constraint(Getter, Target, MaxDistance, Func, Cost, NewConstraint) :-
    wrap_core(lev_core(Target, MaxDistance), Getter, Func, Cost, NewConstraint).

seq_core(Sequence, String, 0.0) :-
    sequence_match(Sequence, String).
subsequence_constraint(Getter, Sequence, Func, Cost, NewConstraint) :-
    wrap_core(seq_core(Sequence), Getter, Func, Cost, NewConstraint).

:- if(can_use_regex).
regex_core(Regex, String, 0.0) :-
    re_match(Regex, String).
:- else.
regex_core(_, _, _) :-
    throw(
        error(
            unsupported_error,
            context(
                regex_core/3, "Regex not supported for current version"
            )
        )
    ).
:- endif.
regex_constraint(Getter, Regex, Func, Cost, NewConstraint) :-
    wrap_core(regex_core(Regex), Getter, Func, Cost, NewConstraint).

similarity_core(Needle, Source, Cost) :-
    similarity(Source, Needle, Similarity),
    Similarity > 0.8,
    Cost is 1.0 - Similarity, !.
similarity_constraint(Getter, Sequence, Func, Cost, NewConstraint) :-
    wrap_core(similarity_core(Sequence), Getter, Func, Cost, NewConstraint).

sub_similarity_core(Needle, Source, Cost) :-
    sub_similarity(Source, Needle, Similarity),
    Similarity > 0.8,
    Cost is 1.0 - Similarity.
sub_similarity_constraint(Getter, Sequence, Func, Cost, NewConstraint) :-
    wrap_core(sub_similarity_core(Sequence), Getter, Func, Cost, NewConstraint).

fuzzy_substr_core(Source, Needle, Cost) :-
    fuzzy_substr(Source, Needle, Similarity),
    Similarity > 0.8,
    Cost is 1.0 - Similarity.
fuzzy_substr_constraint(Getter, Sequence, Func, Cost, NewConstraint) :-
    wrap_core(fuzzy_substr_core(Sequence), Getter, Func, Cost, NewConstraint).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions for building searches from user input.
match_eq(Eq) :- member(Eq, ["eq", "exact", eq, exact]).
match_lev(Lev) :- member(Lev, ["lev", lev]).
match_substr(Substr) :- member(Substr, ["substr", substr]).
match_subseq(Subseq) :- member(Subseq, ["subseq", subseq]).
match_re(Re) :- member(Re, ["re", re]).
match_sim(Sim) :- member(Sim, ["sim", sim]).
match_sub_sim(Sim) :- member(Sim, ["subsim", subsim]).

match_fuzzy_substr(Substr) :- member(Substr, ["fsubstr", fsubstr]).

%% Get the constraint for the field and method
string_constraint(Getter, String, Eq, string_constraints:equality_constraint(Getter, String)) :-
    match_eq(Eq), !.
string_constraint(Getter, String, Lev, string_constraints:levenshtein_constraint(Getter, String, MaxDis)) :-
    match_lev(Lev), !,
    string_length(String, StrLen),
    MaxDis is round(sqrt(StrLen)).
string_constraint(Getter, String, Substr, string_constraints:substring_constraint(Getter, String)) :-
    match_substr(Substr), !.
string_constraint(Getter, String, Subseq, string_constraints:subsequence_constraint(Getter, String)) :-
    match_subseq(Subseq), !.
:- if(can_use_regex).
string_constraint(Getter, String, Re, string_constraints:regex_constraint(Getter, String)) :-
    match_re(Re), !.
:- endif.
string_constraint(Getter, String, Subseq, string_constraints:similarity_constraint(Getter, String)) :-
    match_sim(Subseq), !.
string_constraint(Getter, String, Subseq, string_constraints:sub_similarity_constraint(Getter, String)) :-
    match_sub_sim(Subseq), !.
string_constraint(Getter, String, Subseq, string_constraints:fuzzy_substr_constraint(Getter, String)) :-
    match_fuzzy_substr(Subseq), !.

%% add_string_constraint(+Field, +String, +Method, +OldConstraints, -NewConstraints).
%% none is used to denote constraints which are not provided
add_string_constraint(_, none, _, Constraint, Constraint) :- !.
add_string_constraint(Getter, String, Method, Rhs, and_constraint(Constraint, Rhs)) :-
    string_constraint(Getter, String, Method, Constraint), !.
