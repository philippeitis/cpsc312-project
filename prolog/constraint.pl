:- module(constraint, [
    func_constraints/4,
    input_constraint/4,
    name_substring_constraint/4,
    doc_substring_constraint/4,
    path_constraints/3,
    cycle_constraint/3,
    length_constraint/3,
    path_output_constraint/3,
    no_constraints_left/1
]).
:- use_module(function).
:- use_module(string_op).

%% list_subset(?List1, ?List2)
% Returns true if List1 is a subset of List2.
list_subset([], _).
list_subset([First|Rest], B) :-
    member(First, B),
    list_subset(Rest, B), !.

%% no_constraint(?, ?, ?Score, ?NewConstraint)
% The empty constraint.
no_constraint(_, _, 0.0, (no_constraint, _)).

substring_constraint(Substring, String, 1.0, _, (no_constraint, _)) :-
    sub_string(String, _, _, _, Substring), !.
substring_constraint(Substring, String, 0.0, ConstraintFn, (ConstraintFn, Substring)) :-
    \+sub_string(String, _, _, _, Substring).

name_substring_constraint(Func, Substring, Score, NewConstraint) :-
    name(Func, Name),
    substring_constraint(Substring, Name, Score, name_substring_constraint, NewConstraint).

doc_substring_constraint(Func, Substring, Score, NewConstraint) :-
    docs(Func, Doc),
    substring_constraint(Substring, Doc, Score, doc_substring_constraint, NewConstraint).


target_similarity(String, (Target, MaxDistance), Score, _, (no_constraint, _)) :-
    levenshtein_distance(Target, String, Distance),
    Distance =< MaxDistance,
    Score is MaxDistance - Distance, !.
target_similarity(_, Args, 0.0, ConstraintFn, (ConstraintFn, Args)) :- !.

name_distance_constraint(Func, Args, Score, NewConstraint) :-
    name(Func, Name),
    target_similarity(Name, Args, Score, name_distance_constraint, NewConstraint).

doc_distance_constraint(Func, Args, Score, NewConstraint) :-
    docs(Func, Doc),
    target_similarity(Doc, Args, Score, doc_distance_constraint, NewConstraint).

has_input(Func, TargetInputs) :-
    inputs(Func, Inputs),
    list_subset(TargetInputs, Inputs).
has_output(Func, TargetOutputs) :-
    outputs(Func, Outputs),
    list_subset(TargetOutputs, Outputs).

input_constraint(Func, Inputs, 0.0, (input_constraint, Outputs)) :-
    has_input(Func, Inputs),
    outputs(Func, Outputs).

%% func_constraints(Func, Constraints, ScoreOut, NewConstraints)
% Tests if Func satisfies all constraints, producing a score for this function,
% ScoreOut, and a set of constraints which follow Constraints, NewConstraints.
func_constraints(Func, Constraints, ScoreOut, NewConstraints) :-
    func_constraints(Func, Constraints, 0.0, ScoreOut, NewConstraints).

func_constraints(_Func, [], Score, Score, _) :- !.

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


%% no_constraints_left(Constraints)
% Produces true if all constraints are satisfied, or would be satisfied
% in conjunction with path_output_constraint.
no_constraints_left([]) :- !.
no_constraints_left([(no_constraint, _)|Rest]) :- 
    no_constraints_left(Rest), !.
no_constraints_left([(input_constraint, _)|Rest]) :- 
    no_constraints_left(Rest), !.

%% path_constraints(Path, Func, Constraints)
% Evaluates all of the provided path-level constraints on the path.
path_constraints(_, _, []) :- !.

path_constraints(Path, Func, [(ConstraintFn, Args)|Rest]) :-
    call(ConstraintFn, Path, Func, Args),
    path_constraints(Path, Func, Rest), !.

% Contraint *args, Func, Score
% Constraint -> Score
% Constraint, Func -> Optional(Constraint)
% Path -> Length, membership

% Constraint: Func -> Args -> Score
%% cycle_constraint(?Path, ?Func, ?)
% Produces true if adding Func to Path will not introduce a cycle.
cycle_constraint(_, none, _) :- !.
cycle_constraint(Path, Func, _) :-
    \+member(Func, Path).

%% length_constraint(?Path, ?Func, ?Length:int)
% Produces true if adding Func to Path will not cause Path's length to
% exceed Length.
length_constraint(Path, none, Length) :-
    length(Path, PathLength),
    PathLength =< Length, !.
length_constraint(Path, _, Length) :-
    length(Path, PathLength),
    PathLength < Length, !.

%% path_output_constraint(?Path, ?Func, ?Length:int)
% Produces true if the last function in Path produces OutputTypes.
path_output_constraint([LastFn|_], none, OutputTypes) :-
    has_output(LastFn, OutputTypes).
path_output_constraint(_, Value, _) :- \+(Value=none).
