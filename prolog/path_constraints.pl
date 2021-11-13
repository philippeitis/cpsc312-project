:- module(path_constraints, [
    path_constraints/3,
    cycle_constraint/3,
    length_constraint/3,
    path_output_constraint/3,
    no_constraints_left/1
]).
:- use_module(func_constraints).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Path Constraint Common API
% Path, Func, Args

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

%% cycle_constraint(Path, Func, _)
% Produces true if adding Func to Path will not introduce a cycle.
cycle_constraint(_, none, _) :- !.
cycle_constraint(Path, Func, _) :-
    \+member(Func, Path).

%% length_constraint(Path, Func, Length:int)
% Produces true if adding Func to Path will not cause Path's length to
% exceed Length.
length_constraint(Path, none, Length) :-
    length(Path, PathLength),
    PathLength =< Length, !.
length_constraint(Path, _, Length) :-
    length(Path, PathLength),
    PathLength < Length, !.

%% path_output_constraint(Path, Func, OutputTypes:list)
% Produces true if the last function in Path produces OutputTypes.
path_output_constraint([LastFn|_], none, OutputTypes) :-
    output_constraint(LastFn, OutputTypes, _, _).
path_output_constraint(_, Value, _) :- \+(Value=none).
