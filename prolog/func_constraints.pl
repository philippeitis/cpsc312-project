:- module(func_constraints, [
    input_constraint/4,
    output_constraint/4,
    candidate_fn/1,
    fn_member_constraint/1
]).

:- use_module(constraints, [no_constraint/3, member_constraint/4]).
:- use_module(function).
:- use_module(sequence_ops).

%% Allows visiting specialized functions if necessary. Uses fail to visit a particular function only once.
% Otherwise, we visit the specialized function once (without fail),
% or not at all until the next try (if specialize comes second).
candidate_fn(Fn) :- get_function(_, Fn), generics(Fn, []).
candidate_fn(Fn) :- get_function(_, Parent), specialize(Parent, _, Fn).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Input/output checking

has_input(Fn, TargetInputs) :-
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

%% Creates a member_constraint which checks if a given Fn is in the list of Fns -
% can be used to initialize a path.
fn_member_constraint(constraints:member_constraint(Fns)) :-
    findall(Fn, candidate_fn(Fn), Fns).
