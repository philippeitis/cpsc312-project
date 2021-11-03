type("int").
type("None").

function("parseInt", ["str"], ["int"], "documentation").
function("parseInt2", ["str"], ["int"], "documentation").
function("print", ["int"], ["None"], "documentation").
function("print2", ["int"], ["None"], "documentation").
function("increment", ["int"], ["int"], "documentation").

name(Func, Name) :- function(Func, _, _, _), Name=Func.
inputs(Func, Inputs) :- function(Func, Inputs, _, _).
outputs(Func, Outputs) :- function(Func, _, Outputs, _).
docs(Func, Documentation) :- function(Func, _, _, Documentation).

listSubset([], _).
listSubset([First|Rest], B) :-
    member(First, B),
    listSubset(Rest, B), !.

hasInput(Func, TargetInputs) :-
    inputs(Func, Inputs),
    listSubset(TargetInputs, Inputs).
hasOutput(Func, TargetOutputs) :-
    outputs(Func, Outputs),
    listSubset(TargetOutputs, Outputs).

inputConstraint(Func, Inputs, Score) :-
    hasInput(Func, Inputs),
    Score=1.0, !.

inputConstraint(_Func, _Inputs, Score) :- Score=0.0.

% TODO: Add outputConstraint function
% TODO: Add Levenshtein distance:
% https://en.wikipedia.org/wiki/Levenshtein_distance
% TODO: Add regex?
% TODO: Check if documentation / fn name contains a substring


funcConstraints(Func, Constraints, Threshold, ScoreOut) :-
    funcConstraints(Func, Constraints, Threshold, 1.0, ScoreOut).

funcConstraints(_Func, [], Threshold, ScoreIn, ScoreOut) :-
    ScoreIn > Threshold,
    ScoreOut = ScoreIn, !.

funcConstraints(Func, [pair(ConstraintFn, Args)|Rest], Threshold, ScoreIn, ScoreOut) :-
    ScoreIn > Threshold,
    call(ConstraintFn, Func, Args, ThisScore),
    ScoreIn2 is ScoreIn * ThisScore,
    funcConstraints(Func, Rest, Threshold, ScoreIn2, ScoreOut), !.

funcConstraints(_, _, _, _, Score) :- Score = 0.

% TODO: Make this a breadth-first A* search.
% TODO: Add path length constraint and/or threshold to this.
path(InputTypes, OutputTypes, []) :-
    listSubset(InputTypes, OutputTypes).

% try path(["str"], ["None"], Path). (use ; to get more than one path)
path(InputTypes, OutputTypes, [StartFn|Rest]) :-
    inputs(StartFn, Inputs),
    listSubset(InputTypes, Inputs),
    outputs(StartFn, Outputs),
    path(Outputs, OutputTypes, Rest).

