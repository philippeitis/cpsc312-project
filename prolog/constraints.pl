:- module(constraints, [
    and_constraint/5,
    at_most_n_constraint/5,
    scale_constraint/5,
    no_constraint/3
]).

:- meta_predicate scale_constraint(3, +, +, +, -).
:- meta_predicate and_constraint(3, 3, +, +, -).

%% Function Constraint Common API
% Args*, Cost, NewConstraint

%% no_constraint(?, -Cost, -NewConstraint)
% The empty constraint.
no_constraint(_, 0.0, no_constraint).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Meta Constraints: These apply operations to other constraints

%% and_constraint(+Lhs, +Rhs, -Func, -CostOut, -NewConstraints)
% Tests if Func satisfies all constraints, producing a cost for this function,
% CostOut, and a set of constraints which follow Constraints, NewConstraints.
and_constraint(Lhs, Rhs, Fn, Cost, and_constraint(NewConstraint0, NewConstraint1)) :-
    call(Lhs, Fn, Cost0, NewConstraint0),
    call(Rhs, Fn, Cost1, NewConstraint1),
    Cost is Cost0 + Cost1.

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
