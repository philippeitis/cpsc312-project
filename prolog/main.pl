type("int").
type("None").

function("parseInt", ["str"], ["int"], "documentation").
function("parseInt2", ["str"], ["int"], "documentation").
function("print", ["int"], ["None"], "documentation").
function("print2", ["int"], ["None"], "documentation").

function("increment", ["int"], ["int"], "documentation").

inputs(Func, Inputs) :- function(Func, Inputs, _, _).
outputs(Func, Outputs) :- function(Func, _, Outputs, _).

listSubset([], B, YesOrNo) :- YesOrNo=yes.
listSubset([First|Rest], B, YesOrNo) :-
    member(First, B),
    listSubset(Rest, B, YesOrNo).
listSubset(_, _, YesOrNo) :- YesOrNo=no.

hasInput(Func, TargetInputs, YesOrNo) :-
    inputs(Func, Inputs),
    listSubset(TargetInputs, Inputs, YesOrNo).
hasOutput(Func, TargetOutputs, YesOrNo) :-
    inputs(Func, Outputs),
    listSubset(TargetOutputs, Outputs, YesOrNo).

inputConstraint(Func, Inputs, Score) :-
    hasInput(Func, Inputs, YesOrNo),
    YesOrNo=yes,
    Score=1.0.

inputConstraint(Func, Inputs, Score) :-
    hasInput(Func, Inputs, YesOrNo),
    YesOrNo=no,
    Score=0.0.

funcConstraints(Func, Constraints, Threshold, ScoreOut) :-
    funcConstraints(Func, Constraints, Threshold, 1.0, ScoreOut).

funcConstraints(Func, [pair(ConstraintFn, Args)|Rest], Threshold, ScoreIn, ScoreOut) :-
    ScoreIn > Threshold,
    call(ConstraintFn, Func, Args, ThisScore),
    ScoreIn2 is ScoreIn * ThisScore,
    funcConstraints(Func, Rest, Threshold, ScoreIn2, ScoreOut).

funcConstraints(Func, [], Threshold, ScoreIn, ScoreOut) :-
    ScoreIn > Threshold,
    ScoreOut = ScoreIn.

funcConstraints(_, _, _, _, Score) :-
    Score = 0.

path(InputTypes, OutputTypes, []) :-
    listSubset(InputTypes, OutputTypes, yes).

% try path(["str"], ["None"], Path). (use ; to get more than one path)
path(InputTypes, OutputTypes, [StartFn|Rest]) :-
    inputs(StartFn, Inputs),
    listSubset(InputTypes, Inputs, yes),
    outputs(StartFn, Outputs),
    path(Outputs, OutputTypes, Rest).
