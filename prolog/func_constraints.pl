:- module(func_constraints, [
    input_constraint/4,
    output_constraint/4
]).

:- use_module(constraints, [no_constraint/3]).
:- use_module(function).
:- use_module(sequence_ops).

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
