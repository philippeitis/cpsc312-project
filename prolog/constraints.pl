:- module(constraints, [
    and_constraint/5,
    at_most_n_constraint/5,
    scale_constraint/5,
    no_constraint/3,
    cycle_constraint/5,
    no_constraints_left/1,
    member_constraint/4
]).

:- use_module(function).
:- meta_predicate scale_constraint(3, +, +, -, -).
:- meta_predicate and_constraint(3, 3, +, -, -).
:- meta_predicate cycle_constraint(2, +, +, -, -).

%% no_constraints_left(Constraints)
% Produces true if all constraints are satisfied, or would be satisfied
% in conjunction with path_output_constraint.
no_constraints_left(and_constraint(Lhs, Rhs)) :-
    no_constraints_left(Lhs),
    no_constraints_left(Rhs).
no_constraints_left(no_constraint).
no_constraints_left(input_constraint(_)).
no_constraints_left(at_most_n_constraint(_, _)).
no_constraints_left(cycle_constraint(_, _)).
no_constraints_left(constraints:cycle_constraint(_, _)).
no_constraints_left(member_constraint(_)).
no_constraints_left(constraints:member_constraint(_)).

%% Constraint Common API
% Args*, Cost, NewConstraint

%% no_constraint(?, -Cost, -NewConstraint)
% The empty constraint.
no_constraint(_, 0.0, no_constraint).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Meta Constraints: These apply operations to other constraints

%! and_constraint(+Lhs, +Rhs, -Func, -CostOut, -NewConstraints)
% Tests if Func satisfies all constraints, producing a cost for this function,
% CostOut, and a set of constraints which follow Constraints, NewConstraints.
and_constraint(Lhs, Rhs, Fn, Cost, and_constraint(NewConstraint0, NewConstraint1)) :-
    call(Lhs, Fn, Cost0, NewConstraint0),
    call(Rhs, Fn, Cost1, NewConstraint1),
    Cost is Cost0 + Cost1.

%! at_most_n_constraint(+N, +Constraint, -Func, -Cost, -NewConstraint)
% Evaluates at most N+1 times (if base case is reached, )
at_most_n_constraint(N, Constraint, Func, Cost, at_most_n_constraint(NSub, Constraint)) :-
    call(Constraint, Func, Cost, _), !,
    NSub is N - 1,
    NSub >= 0.

at_most_n_constraint(N, Constraint, _, 0.0, at_most_n_constraint(N, Constraint)) :- N >= 0.

%% Negates the result of evaluating Constraint
not_constraint(Constraint, Fn, 0.0, no_constraint) :-
    \+call(Constraint, Fn, _, _).

%% Scales the cost of the given constraint, and all future constraints in this path by this amount.
scale_constraint(Constraint, Weight, Fn, Scaled, scale_constraint(NewConstraint, Weight)) :-
    call(Constraint, Fn, Cost, NewConstraint),
    Scaled is Cost * Weight.

%% Getter should be a function that produces produces a hash-like value. This value is then used to
%% check that the item has not previously been added to the path.
cycle_constraint(Getter, Previous, Item, 0.0, constraints:cycle_constraint(Getter, Next)) :-
    call(Getter, Item, Key),
    \+ord_memberchk(Key, Previous),
    ord_add_element(Previous, Key, Next).

%% Checks if the item is a member of the list - can be used to instantiate constraints when performing
%% path generation
member_constraint(List, Item, 0.0, constraints:member_constraint(List)) :-
    member(Item, List).