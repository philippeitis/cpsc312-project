:- module(path_constraints, [
    and_constraint/3,
    cycle_constraint/2,
    length_constraint/3,
    output_constraint/3,
    no_constraints_left/1
]).
:- use_module(func_constraints).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Path Constraint Common API
% Args*, Path, Func

%% no_constraints_left(Constraints)
% Produces true if all constraints are satisfied, or would be satisfied
% in conjunction with path_output_constraint.
no_constraints_left(and_constraint(Constraints)) :-
    forall(
        member(Constraint, Constraints),
        no_constraints_left(Constraint)
    ), !.
no_constraints_left(no_constraint) :- !.
no_constraints_left(input_constraint(_)) :- !.

%% and_constraint(+Constraints, +Path, +Func)
% Evaluates all of the provided path-level constraints on the path.
and_constraint(Constraints, Path, Func) :-
    forall(
        member(Constraint, Constraints),
        call(Constraint, Path, Func)
    ).

%% cycle_constraint(Path, Func, _)
% Produces true if adding Func to Path will not introduce a cycle.
cycle_constraint(_, none) :- !.
cycle_constraint(Path, Func) :-
    \+member(Func, Path).

%% length_constraint(Path, Func, Length:int)
% Produces true if adding Func to Path will not cause Path's length to
% exceed Length.
length_constraint(Length, Path, none) :-
    length(Path, PathLength),
    PathLength =< Length, !.
length_constraint(Length, Path, _) :-
    length(Path, PathLength),
    PathLength < Length, !.

%% output_constraint(?OutputTypes:list, +Path:list, +Func)
% Produces true if the last function in Path produces OutputTypes.
output_constraint(OutputTypes, [LastFn|_], none) :-
    output_constraint(OutputTypes, LastFn, _, _).
output_constraint(_, _, Value) :- \+(Value=none).
