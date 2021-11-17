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

:- use_module(compat).
:- use_module(function).
:- use_module(sequence_ops).
:- if(prolog_version_eight).
:- use_module(library(pcre)).
:- endif.

%% Function Constraint Common API
% Func, Args, Cost, NewConstraint

%% no_constraint(?, ?, ?Cost, ?NewConstraint)
% The empty constraint.
no_constraint(_, _, 0.0, (no_constraint, _)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Wrappers for string comparision methods
wrapper(Func, (Self, Args), Cost, NewConstraint) :-
    wrap_core(Self, Func, Args, Cost, NewConstraint).

wrap_core(Core, Func, Args, Cost, (no_constraint, _)) :-
    call(Core, Func, Args, Cost), !.

wrap_core(Core, Func, Args, 1.0, (wrapper, (Core, Args))) :-
    \+call(Core, Func, Args, _), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% String comparison methods
% x_core: Func, Args, Cost
% x_constraint: Func, Args, Cost, NewConstraint
equality_core(Func, (Target, Field), 0.0) :-
    get_field(Func, Field, Target), !.
equality_constraint(Func, Args, Cost, NewConstraint) :-
    wrap_core(equality_core, Func, Args, Cost, NewConstraint).

sub_core(Func, (Needle, Field), 0.0) :-
    get_field(Func, Field, String),
    sub_string(String, _, _, _, Needle), !.
substring_constraint(Func, Args, Cost, NewConstraint) :-
    wrap_core(sub_core, Func, Args, Cost, NewConstraint).

lev_core(Func, (Target, Field, MaxDistance), Distance) :-
    get_field(Func, Field, String),
    levenshtein_distance(Target, String, Distance),
    Distance =< MaxDistance, !.
levenshtein_constraint(Func, Args, Cost, NewConstraint) :-
    wrap_core(lev_core, Func, Args, Cost, NewConstraint).

seq_core(Func, (Sequence, Field), 0.0) :-
    get_field(Func, Field, String),
    sequence_match(Sequence, String), !.
subsequence_constraint(Func, Args, Cost, NewConstraint) :-
    wrap_core(seq_core, Func, Args, Cost, NewConstraint).

:- if(prolog_version_eight).
regex_core(Func, (Regex, Field), 0.0) :-
    get_field(Func, Field, String),
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
regex_constraint(Func, Args, Cost, NewConstraint) :-
    wrap_core(regex_core, Func, Args, Cost, NewConstraint).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Input/output checking
inputs_to_interp([], []).
inputs_to_interp([Input|Inputs], [(_, Input)|Rest]) :-
    inputs_to_interp(Inputs, Rest).

%% Specialize the function if necessary, and fail so that
% we only visit this function once.
% Otherwise, we vist the specialized function once (without fail),
% or not at all until the next try (if specialize comes second).
% Keeps specialized functions around.
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
:- if(prolog_version_eight).
field_constraint(Field, String, Re, (regex_constraint, (String, Field))) :-
    match_re(Re), !.
:- endif.

%% none is used to denote constraints which are not provided
add_field_constraint(_, none, _, Constraints, Constraints) :- !.
add_field_constraint(Field, String, Method, Constraints, [Constraint|Constraints]) :-
    field_constraint(Field, String, Method, Constraint), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% func_constraints(Func, Constraints, CostOut, NewConstraints)
% Tests if Func satisfies all constraints, producing a cost for this function,
% CostOut, and a set of constraints which follow Constraints, NewConstraints.
func_constraints(Func, Constraints, CostOut, NewConstraints) :-
    func_constraints(Func, Constraints, 0.0, CostOut, NewConstraints).

func_constraints(_Func, [], Cost, Cost, []) :- !.

func_constraints(
    Func,
    [(ConstraintFn, Args)|Rest],
    CostIn,
    CostOut,
    [NewConstraint|NewConstraints]
    ) :-
    call(ConstraintFn, Func, Args, ThisCost, NewConstraint),
    CostIn2 is CostIn + ThisCost,
    func_constraints(Func, Rest, CostIn2, CostOut, NewConstraints).
