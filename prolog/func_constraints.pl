:- module(func_constraints, [
    and_constraint/4,
    add_field_constraint/5,
    input_constraint/4,
    output_constraint/4,
    equality_constraint/5,
    substring_constraint/5,
    levenshtein_constraint/6,
    subsequence_constraint/5,
    regex_constraint/5,
    similarity_constraint/5,
    sub_similarity_constraint/5,
    fuzzy_substr_constraint/5,
    at_most_n_constraint/5,
    scale_constraint/5
]).

:- use_module(compat).
:- use_module(function).
:- use_module(sequence_ops).
:- use_module(nlp).

%% Function Constraint Common API
% Args*, Cost, NewConstraint

%% no_constraint(?, -Cost, -NewConstraint)
% The empty constraint.
no_constraint(_, 0.0, no_constraint).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Wrappers for string comparison methods

wrapper(Constraint, Field, Func, Cost, NewConstraint) :-
    wrap_core(Constraint, Field, Func, Cost, NewConstraint).

wrap_core(Constraint, Field, Func, Cost, no_constraint) :-
    get_field(Func, Field, Value),
    call(Constraint, Value, Cost), !.

wrap_core(Constraint, Field, Func, 1.0, wrapper(Constraint, Field)) :-
    get_field(Func, Field, Value),
    \+call(Constraint, Value, _), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% String comparison methods
% x_core: Args*, Value, Cost
% x_constraint: Args*, Func, Cost, NewConstraint

equality_core(Target, Target, 0.0).
equality_constraint(Target, Field, Func, Cost, NewConstraint) :-
    wrap_core(equality_core(Target), Field, Func, Cost, NewConstraint).

sub_core(Needle, String, 0.0) :-
    sub_string(String, _, _, _, Needle), !.
substring_constraint(Needle, Field, Func, Cost, NewConstraint) :-
    wrap_core(sub_core(Needle), Field, Func, Cost, NewConstraint).

lev_core(Target, MaxDistance, String, Distance) :-
    levenshtein_distance(Target, String, LevDistance),
    LevDistance =< MaxDistance,
    Distance is LevDistance / MaxDistance, !.
levenshtein_constraint(Target, Field, MaxDistance, Func, Cost, NewConstraint) :-
    wrap_core(lev_core(Target, MaxDistance), Field, Func, Cost, NewConstraint).

seq_core(Sequence, String, 0.0) :-
    sequence_match(Sequence, String), !.
subsequence_constraint(Sequence, Field, Func, Cost, NewConstraint) :-
    wrap_core(seq_core(Sequence), Field, Func, Cost, NewConstraint).

:- if(can_use_regex).
regex_core(Regex, String, 0.0) :-
    re_match(Regex, String), !.
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
regex_constraint(Regex, Field, Func, Cost, NewConstraint) :-
    wrap_core(regex_core(Regex), Field, Func, Cost, NewConstraint).

similarity_core(Needle, Source, Cost) :-
    similarity(Source, Needle, Similarity),
    Similarity > 0.8,
    Cost is 1.0 - Similarity, !.
similarity_constraint(Sequence, Field, Func, Cost, NewConstraint) :-
    wrap_core(similarity_core(Sequence), Field, Func, Cost, NewConstraint).

sub_similarity_core(Needle, Source, Cost) :-
    sub_similarity(Source, Needle, Similarity),
    Similarity > 0.8,
    Cost is 1.0 - Similarity, !.
sub_similarity_constraint(Sequence, Field, Func, Cost, NewConstraint) :-
    wrap_core(sub_similarity_core(Sequence), Field, Func, Cost, NewConstraint).

fuzzy_substr_core(Source, Needle, Cost) :-
    fuzzy_substr(Source, Needle, Similarity),
    Similarity > 0.8,
    Cost is 1.0 - Similarity, !.
fuzzy_substr_constraint(Sequence, Field, Func, Cost, NewConstraint) :-
    wrap_core(fuzzy_substr_core(Sequence), Field, Func, Cost, NewConstraint).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Input/output checking

%% Specialize the function if necessary, and fail so that
% we only visit this function once.
% Otherwise, we vist the specialized function once (without fail),
% or not at all until the next try (if specialize comes second).
% Keeps specialized functions around.
candidate_fn(Func) :- specialize(_, _, Func), fail.
candidate_fn(Func) :- generics(Func, []).

has_input(Fn, TargetInputs) :-
    candidate_fn(Fn),
    inputs(Fn, Inputs),
    list_subset(TargetInputs, Inputs).

input_constraint(Inputs, Fn, 0.0, input_constraint(Outputs)) :-
    has_input(Fn, Inputs),
    outputs(Fn, Outputs).

has_output(Fn, TargetOutputs) :-
    outputs(Fn, Outputs),
    list_subset(TargetOutputs, Outputs).

output_constraint(Outputs, Fn, 0.0, no_constraint) :-
    has_output(Fn, Outputs).

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
field_constraint(Field, String, Eq, func_constraints:equality_constraint(String, Field)) :-
    match_eq(Eq), !.
field_constraint(Field, String, Lev, func_constraints:levenshtein_constraint(String, Field, MaxDis)) :-
    match_lev(Lev), !, string_length(String, MaxDis).
field_constraint(Field, String, Substr, func_constraints:substring_constraint(String, Field)) :-
    match_substr(Substr), !.
field_constraint(Field, String, Subseq, func_constraints:subsequence_constraint(String, Field)) :-
    match_subseq(Subseq), !.
:- if(can_use_regex).
field_constraint(Field, String, Re, func_constraints:regex_constraint(String, Field)) :-
    match_re(Re), !.
:- endif.
field_constraint(Field, String, Subseq, func_constraints:similarity_constraint(String, Field)) :-
    match_sim(Subseq), !.
field_constraint(Field, String, Subseq, func_constraints:sub_similarity_constraint(String, Field)) :-
    match_sub_sim(Subseq), !.
field_constraint(Field, String, Subseq, func_constraints:fuzzy_substr_constraint(String, Field)) :-
    match_fuzzy_substr(Subseq), !.

%% add_field_constraint(+Field, +String, +Method, +OldConstraints, -NewConstraints).
%% none is used to denote constraints which are not provided
add_field_constraint(_, none, _, Constraints, Constraints) :- !.
add_field_constraint(Field, String, Method, Constraints, [Constraint|Constraints]) :-
    field_constraint(Field, String, Method, Constraint), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Meta Constraints: These apply operations to other constraints

%% and_constraint(+Constraints, -Func, -CostOut, -NewConstraints)
% Tests if Func satisfies all constraints, producing a cost for this function,
% CostOut, and a set of constraints which follow Constraints, NewConstraints.
and_constraint(Constraints, Fn, Cost, and_constraint(NewConstraints)) :-
    and_constraint(Constraints, Fn, 0.0, Cost, NewConstraints).

and_constraint([], _, Cost, Cost, []) :- !.

and_constraint(
    [Constraint|Rest],
    Fn,
    CostIn,
    CostOut,
    [NewConstraint|NewConstraints]
    ) :-
    call(Constraint, Fn, Cost, NewConstraint),
    NextCost is CostIn + Cost,
    and_constraint(Rest, Fn, NextCost, CostOut, NewConstraints).

%% at_most_n_constraint(+N, +Constraint, -Func, -Cost, -NewConstraint)
% Evaluates at most N+1 times (if base case is reached, )
at_most_n_constraint(N, Constraint, Func, Cost, at_most_n_constraint(NSub, Constraint)) :-
    call(Constraint, Func, Cost, _), !,
    NSub is N - 1,
    NSub >= 0.

at_most_n_constraint(N, Constraint, _, 0.0, at_most_n_constraint(N, Constraint)) :- N >= 0.

not_constraint(Constraint, Fn, 0.0, no_constraint) :-
    \+call(Constraint, Fn, _, _).

scale_constraint(Constraint, Weight, Fn, Scaled, scale_constraint(NewConstraint, Weight)) :-
    call(Constraint, Fn, Cost, NewConstraint),
    Scaled is Cost * Weight.
