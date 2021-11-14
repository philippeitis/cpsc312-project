:- module(func_constraints, [
    func_constraints/4,
    add_field_constraint/5,
    input_constraint/4,
    output_constraint/4,
    equality_constraint/4,
    substring_constraint/4,
    levenshtein_constraint/4,
    subsequence_constraint/4,
    regex_constraint/4
]).
:- use_module(function).
:- use_module(string_op).
:- use_module(library(pcre)).

%% Function Constraint Common API
% Func, Args, Score, NewConstraint

%% list_subset(?List1, ?List2)
% Returns true if List1 is a subset of List2.
list_subset([], _).
list_subset([First|Rest], B) :-
    member(First, B),
    list_subset(Rest, B), !.

%% no_constraint(?, ?, ?Score, ?NewConstraint)
% The empty constraint.
no_constraint(_, _, 0.0, (no_constraint, _)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Wrappers for string comparision methods
wrapper(Func, (Self, Args), Score, NewConstraint) :-
    wrap_core(Self, Func, Args, Score, NewConstraint).

wrap_core(Core, Func, Args, Score, (no_constraint, _)) :-
    call(Core, Func, Args, Score), !.

wrap_core(Core, Func, Args, 0.0, (wrapper, (Core, Args))) :-
    \+call(Core, Func, Args, _), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% String comparison methods
% x_core: Func, Args, Score
% x_constraint: Func, Args, Score, NewConstraint
equality_core(Func, (Target, Field), 1.0) :-
    get_field(Func, Field, Target), !.
equality_constraint(Func, Args, Score, NewConstraint) :-
    wrap_core(equality_core, Func, Args, Score, NewConstraint).

sub_core(Func, (Needle, Field), 1.0) :-
    get_field(Func, Field, String),
    sub_string(String, _, _, _, Needle), !.
substring_constraint(Func, Args, Score, NewConstraint) :-
    wrap_core(sub_core, Func, Args, Score, NewConstraint).

lev_core(Func, (Target, Field, MaxDistance), Score) :-
    get_field(Func, Field, String),
    levenshtein_distance(Target, String, Distance),
    Distance =< MaxDistance,
    Score is MaxDistance - Distance, !.
levenshtein_constraint(Func, Args, Score, NewConstraint) :-
    wrap_core(lev_core, Func, Args, Score, NewConstraint).

seq_core(Func, (Sequence, Field), 1.0) :-
    get_field(Func, Field, String),
    sequence_match(Sequence, String), !.
subsequence_constraint(Func, Args, Score, NewConstraint) :-
    wrap_core(seq_core, Func, Args, Score, NewConstraint).

regex_core(Func, (Regex, Field), 1.0) :-
    get_field(Func, Field, String),
    re_match(Regex, String).

regex_constraint(Func, Args, Score, NewConstraint) :-
    wrap_core(regex_core, Func, Args, Score, NewConstraint).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Input/output checking
inputs_to_interp([], []).
inputs_to_interp([Input|Inputs], [(_, Input)|Rest]) :-
    inputs_to_interp(Inputs, Rest).

%% Specialize the function if necessary, and fail so that
% we only visit this function once.
% Otherwise, we vist the specialized function once (without fail),
% or not at all until the next try (if specialize comes second).
candidate_fn(Func) :- specialize(_, _, Func), fail.
candidate_fn(Func) :- generics(Func, []).

has_input(Func, TargetInputs) :-
    candidate_fn(Func),
    inputs(Func, Inputs),
    list_subset(TargetInputs, Inputs).

input_constraint(Func, Inputs, 0.0, (input_constraint, Outputs)) :-
    has_input(Func, Inputs),
    outputs(Func, Outputs).

has_output(Func, TargetOutputs) :-
    outputs(Func, Outputs),
    list_subset(TargetOutputs, Outputs).

output_constraint(Func, Outputs, 0.0, (no_constraint, _)) :-
    has_output(Func, Outputs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions for building searches from user input.
match_eq(Eq) :- member(Eq, ["eq", "exact", eq, exact]).
match_lev(Lev) :- member(Lev, ["lev", lev]).
match_substr(Substr) :- member(Substr, ["substr", substr]).
match_subseq(Subseq) :- member(Subseq, ["subseq", subseq]).
match_re(Re) :- member(Re, ["re", re]).

%% Get the constraint for the field and method
field_constraint(Field, String, Eq, (equality_constraint, (String, Field))) :-
    match_eq(Eq), !.
field_constraint(Field, String, Lev, (levenshtein_constraint, (String, Field, MaxDis))) :-
    match_lev(Lev), !, string_length(String, MaxDis).
field_constraint(Field, String, Substr, (substring_constraint, (String, Field))) :-
    match_substr(Substr), !.
field_constraint(Field, String, Subseq, (subsequence_constraint, (String, Field))) :-
    match_subseq(Subseq), !.
field_constraint(Field, String, Re, (regex_constraint, (String, Field))) :-
    match_re(Re), !.

%% none is used to denote constraints which are not provided
add_field_constraint(_, none, _, Constraints, Constraints) :- !.
add_field_constraint(Field, String, Method, Constraints, [Constraint|Constraints]) :-
    field_constraint(Field, String, Method, Constraint), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% func_constraints(Func, Constraints, ScoreOut, NewConstraints)
% Tests if Func satisfies all constraints, producing a score for this function,
% ScoreOut, and a set of constraints which follow Constraints, NewConstraints.
func_constraints(Func, Constraints, ScoreOut, NewConstraints) :-
    func_constraints(Func, Constraints, 0.0, ScoreOut, NewConstraints).

func_constraints(_Func, [], Score, Score, []) :- !.

func_constraints(
    Func,
    [(ConstraintFn, Args)|Rest],
    ScoreIn,
    ScoreOut,
    [NewConstraint|NewConstraints]
    ) :-
    call(ConstraintFn, Func, Args, ThisScore, NewConstraint),
    ScoreIn2 is ScoreIn + ThisScore,
    func_constraints(Func, Rest, ScoreIn2, ScoreOut, NewConstraints).