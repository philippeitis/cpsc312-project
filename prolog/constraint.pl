:- module(constraint, [
    func_constraints/4,
    add_field_constraint/5,
    input_constraint/4,
    output_constraint/4,
    equality_constraint/4,
    substring_constraint/4,
    levenshtein_constraint/4,
    subsequence_constraint/4,
    regex_constraint/4,
    path_constraints/3,
    cycle_constraint/3,
    length_constraint/3,
    path_output_constraint/3,
    no_constraints_left/1
]).
:- use_module(function).
:- use_module(string_op).
:- use_module(library(pcre)).

%% list_subset(?List1, ?List2)
% Returns true if List1 is a subset of List2.
list_subset([], _).
list_subset([First|Rest], B) :-
    member(First, B),
    list_subset(Rest, B), !.

%% no_constraint(?, ?, ?Score, ?NewConstraint)
% The empty constraint.
no_constraint(_, _, 0.0, (no_constraint, _)).

%% Wrapper for string comparision methods
wrapper(Func, (Self, Args), Score, NewConstraint) :-
    wrap_core(Self, Func, Args, Score, NewConstraint).

wrap_core(Core, Func, Args, Score, (no_constraint, _)) :-
    call(Core, Func, Args, Score), !.

wrap_core(Core, Func, Args, 0.0, (wrapper, (Core, Args))) :-
    \+call(Core, Func, Args, _), !.

%% String comparison methods
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

%% Input/output checking
has_input(Func, TargetInputs) :-
    inputs(Func, Inputs),
    list_subset(TargetInputs, Inputs).
has_output(Func, TargetOutputs) :-
    outputs(Func, Outputs),
    list_subset(TargetOutputs, Outputs).

input_constraint(Func, Inputs, 0.0, (input_constraint, Outputs)) :-
    has_input(Func, Inputs),
    outputs(Func, Outputs).

output_constraint(Func, Outputs, 0.0, (no_constraint, _)) :-
    has_output(Func, Outputs).

%% Helper functions for building searches from user input.
match_eq(Eq) :- member(Eq, ["eq", "exact", eq, exact]).
match_lev(Lev) :- member(Lev, ["lev", lev]).
match_substr(Substr) :- member(Substr, ["substr", substr]).
match_subseq(Subseq) :- member(Subseq, ["subseq", subseq]).
match_re(Re) :- member(Re, ["re", re]).

%% Get the constraint for the field and method
get_field_constraint(Field, String, Eq, (equality_constraint, (String, Field))) :-
    match_eq(Eq), !.
get_field_constraint(Field, String, Lev, (levenshtein_constraint, (String, Field, MaxDis))) :-
    match_lev(Lev), !, string_length(String, MaxDis).
get_field_constraint(Field, String, Substr, (substring_constraint, (String, Field))) :-
    match_substr(Substr), !.
get_field_constraint(Field, String, Subseq, (subsequence_constraint, (String, Field))) :-
    match_subseq(Subseq), !.
get_field_constraint(Field, String, Re, (regex_constraint, (String, Field))) :-
    match_re(Re), !.

%% none is used to denote constraints which are not provided
add_field_constraint(_, none, _, Constraints, Constraints) :- !.
add_field_constraint(Field, String, Method, Constraints, [Constraint|Constraints]) :-
    get_field_constraint(Field, String, Method, Constraint), !.

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
