type("int").
type("None").

% Function is fnIdentifier, fnInputs, fnOutputs, fnDocs
% fnIdentifier: Currently function name, but we might use an unique identifier
% fnInputs: List of function arguments,
% fnOutputs: List of function outputs,
% fnDocs: User documentation for function
function("parseInt", ["str"], ["int"], "Realises the popular combination of atom_codes/2 and number_codes/2 to convert between atom and number (integer, float or non-integer rational) in one predicate, avoiding the intermediate list. Unlike the ISO standard number_codes/2 predicates, atom_number/2 fails silently in mode (+,-) if Atom does not represent a number.").
function("parseInt2", ["str"], ["int"],"documentation").
function("print", ["int"], ["None"], "Print a term for debugging purposes. The predicate print/1 acts as if defined as below. The print/1 predicate is used primarily through the ~p escape sequence of format/2, which is commonly used in the recipes used by print_message/2 to emit messages. The classical definition of this predicate is equivalent to the ISO predicate write_term/2 using the options portray(true) and numbervars(true). The portray(true) option allows the user to implement application-specific printing of terms printed during debugging to facilitate easy understanding of the output. See also portray/1 and library(portray_text). SWI-Prolog adds quoted(true) to (1) facilitate the copying/pasting of terms that are not affected by portray/1 and to (2) allow numbers, atoms and strings to be more easily distinguished, e.g., 42, '42' and 42.").
function("print2", ["int"], ["None"], "documentation").
function("increment", ["int"], ["int"], "The increment function, or inc, or incr, will take an integer, or int, and increase its value by 1, or add 1 to it and then return the sum.").
function("decrement", ["int"], ["int"], "The decrement function or dec, or decr, will take an integer, or int, and decrease its value by 1, or subtract 1 from it and then return the difference.").
function("sum", ["List[int]"], ["int"], "Sum, or add integers, or add ints, will take in a list of integers or ints, and adds up all of the numerical values in the list. It returns a single integer which is the sum of this addition.").

name(Func, Name) :- function(Func, _, _, _), Name=Func.
inputs(Func, Inputs) :- function(Func, Inputs, _, _).
outputs(Func, Outputs) :- function(Func, _, Outputs, _).
docs(Func, Documentation) :- function(Func, _, _, Documentation).

listSubset([], _).
listSubset([First|Rest], B) :-
    member(First, B),
    listSubset(Rest, B), !.

substring(Substring, String) :-
    string_concat(Substring, _, Suffix),
    string_concat(_, Suffix, Name).

substringConstraint(Substring, String, Score) :-
  substring(Substring, String),
  Score = 1.0.

substringConstraint(_, _, Score) :- Score = 0.0.

nameSubstringConstraint(Func, Substring, Score) :-
    name(Func, Name),
    substringConstraint(Substring, Name, Score).

docSubstringConstraint(Func, Substring, Score) :-
    docs(Func, Doc),
    substringConstraint(Substring, Doc, Score).

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

outputConstraint(Func, Inputs, Score) :-
    hasOutput(Func, Inputs),
    Score=1.0, !.

outputConstraint(_Func, _Inputs, Score) :- Score=0.0.

% TODO: Add Levenshtein distance:
% https://en.wikipedia.org/wiki/Levenshtein_distance
% TODO: Add regex?
% TODO: Check if documentation / fn name contains a substring


funcConstraints(Func, Constraints, Threshold, ScoreOut) :-
    funcConstraints(Func, Constraints, Threshold, 1.0, ScoreOut).

funcConstraints(_Func, [], Threshold, ScoreIn, ScoreOut) :-
    ScoreIn > Threshold,
    ScoreOut = ScoreIn, !.

funcConstraints(Func, [(ConstraintFn, Args)|Rest], Threshold, ScoreIn, ScoreOut) :-
    ScoreIn > Threshold,
    call(ConstraintFn, Func, Args, ThisScore),
    ScoreIn2 is ScoreIn * ThisScore,
    funcConstraints(Func, Rest, Threshold, ScoreIn2, ScoreOut), !.

funcConstraints(_, _, _, _, Score) :- Score = 0.

pathConstraints(_, _, []).

pathConstraints(Path, Func, [(ConstraintFn, Args)|Rest]) :-
    call(ConstraintFn, Path, Func, Args),
    pathConstraints(Path, Func, Rest), !.

% try funcPath(["str"], ["None"], Path). (use ; to get more than one path)
funcPath(InputTypes, OutputTypes, Path) :-
    funcPath(InputTypes, OutputTypes, [], [(lengthConstraint, 999)], Path).

% TODO: Make this a breadth-first search which expands highest priority
% items first (eg. items with highest score) - look @ A*.
% TODO: Add path length constraint and/or threshold to this.

funcPathNoCycles(InputTypes, OutputTypes, Path) :-
    funcPath(InputTypes, OutputTypes, [],  [
        (cycleConstraint, _),
        (lengthConstraint, 999)
    ], Path).

funcPath(InputTypes, OutputTypes, Visited, PathConstraints, []) :-
    listSubset(InputTypes, OutputTypes),
    pathConstraints(Visited, none, PathConstraints).

funcPath(InputTypes, OutputTypes, Visited, PathConstraints, [StartFn|Rest]) :-
    inputs(StartFn, Inputs),
    listSubset(InputTypes, Inputs),
    outputs(StartFn, Outputs),
    pathConstraints(Visited, StartFn, PathConstraints),
    funcPath(Outputs, OutputTypes, [StartFn|Visited], PathConstraints, Rest).

% Contraint *args, Func, Score
% Constraint -> Score
% Constraint, Func -> Optional(Constraint)
% Path -> Length, membership

% Constraint: Func -> Args -> Score
cycleConstraint(_, Func, _) :- Func = none.
cycleConstraint(Path, Func, _) :-
    \+member(Func, Path).

lengthConstraint(Path, _, Length) :-
    length(Path, PathLength),
    PathLength =< Length.
