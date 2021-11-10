:- dynamic function/4.

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

noConstraint(_, _, 0.0, (noConstraint, _)).

substringConstraint(Substring, String, 1.0, _, (noConstraint, _)) :-
    sub_string(String, _, _, _, Substring), !.
substringConstraint(Substring, String, 0.0, ConstraintFn, (ConstraintFn, Substring)) :-
    \+sub_string(String, _, _, _, Substring).

nameSubstringConstraint(Func, Substring, Score, NewConstraint) :-
    name(Func, Name),
    substringConstraint(Substring, Name, Score, nameSubstringConstraint, NewConstraint).

docSubstringConstraint(Func, Substring, Score, NewConstraint) :-
    docs(Func, Doc),
    substringConstraint(Substring, Doc, Score, docSubstringConstraint, NewConstraint).


hasInput(Func, TargetInputs) :-
    inputs(Func, Inputs),
    listSubset(TargetInputs, Inputs).
hasOutput(Func, TargetOutputs) :-
    outputs(Func, Outputs),
    listSubset(TargetOutputs, Outputs).

inputConstraint(Func, Inputs, 0.0, NewConstraint) :-
    hasInput(Func, Inputs),
    outputs(Func, Outputs),
    NewConstraint = (inputConstraint, Outputs).

% TODO: Add Levenshtein distance:
% https://en.wikipedia.org/wiki/Levenshtein_distance
% TODO: Add regex?

funcConstraints(Func, Constraints, ScoreOut, NewConstraints) :-
    funcConstraints(Func, Constraints, 0.0, ScoreOut, NewConstraints).

funcConstraints(_Func, [], ScoreIn, ScoreOut, _) :-
    ScoreOut = ScoreIn, !.

funcConstraints(
    Func,
    [(ConstraintFn, Args)|Rest],
    ScoreIn,
    ScoreOut,
    [NewConstraint|NewConstraints]
    ) :-
    call(ConstraintFn, Func, Args, ThisScore, NewConstraint),
    ScoreIn2 is ScoreIn + ThisScore,
    funcConstraints(Func, Rest, ScoreIn2, ScoreOut, NewConstraints).

pathConstraints(_, _, []) :- !.

pathConstraints(Path, Func, [(ConstraintFn, Args)|Rest]) :-
    call(ConstraintFn, Path, Func, Args),
    pathConstraints(Path, Func, Rest), !.

% try funcPath(["str"], ["None"], Path). (use ; to get more than one path)
funcPath(InputTypes, OutputTypes, Path) :-
    funcPath(
        [],
        [(inputConstraint, InputTypes)],
        [(lengthConstraint, 999),
         (pathOutputConstraint, OutputTypes)],
        Path
    ).
% TODO: Make this a breadth-first search which expands highest priority
% items first (eg. items with highest score) - look @ A*.
% TODO: Add path length constraint and/or threshold to this.

funcPathNoCycles(InputTypes, OutputTypes, Path) :-
    funcPath(
        [],
        [(inputConstraint, InputTypes)],
        [(cycleConstraint, _),
        (lengthConstraint, 999),
        (pathOutputConstraint, OutputTypes)], Path).

funcPath(Visited, FuncConstraints, PathConstraints, []) :-
    pathConstraints(Visited, none, PathConstraints),
    noConstraintsLeft(FuncConstraints).

funcPath(Visited, FuncConstraints, PathConstraints, [StartFn|Rest]) :-
    funcConstraints(StartFn, FuncConstraints, _, NewConstraints),
    pathConstraints(Visited, StartFn, PathConstraints),
    funcPath([StartFn|Visited], NewConstraints, PathConstraints, Rest).


% Contraint *args, Func, Score
% Constraint -> Score
% Constraint, Func -> Optional(Constraint)
% Path -> Length, membership

% Constraint: Func -> Args -> Score
cycleConstraint(_, none, _) :- !.
cycleConstraint(Path, Func, _) :-
    \+member(Func, Path).

lengthConstraint(Path, _, Length) :-
    length(Path, PathLength),
    PathLength =< Length.

pathOutputConstraint([LastFn|_], none, OutputTypes) :-
    hasOutput(LastFn, OutputTypes).
pathOutputConstraint(_, Value, _) :- \+(Value=none).

% Needs to be used in conjunction with pathOutputConstraint
noConstraintsLeft([]) :- !.
noConstraintsLeft([(noConstraint, _)|Rest]) :- 
    noConstraintsLeft(Rest).
noConstraintsLeft([(inputConstraint, _)|Rest]) :- 
    noConstraintsLeft(Rest).

split_left(String, Sep, N, Substrings) :-
    string_chars(String, Chars),
    string_chars(Sep, Sep_),
    split_left(Chars, Sep_, N, [], CharSubstrings),
    maplist(
        string_chars,
        Substrings,
        CharSubstrings
    ), !.

% [Char], [Sep], N, [Acc], [Reversed]
split_left([], _Sep, _, Accumulator, [Reversed]) :-
    reverse(Accumulator, Reversed), !.
split_left([Head|Tail], Sep, 0, Accumulator, [Whole]) :-
    member(Head, Sep),
    split_left(Tail, Sep, 0, Accumulator, [Whole]), !.
split_left(String, _Sep, 0, Accumulator, [Whole]) :-
    reverse(Accumulator, Reversed),
    append(Reversed, String, Whole), !.
split_left([Head|Tail], Sep, N, [], Strings) :-
    member(Head, Sep),
    split_left(Tail, Sep, N, [], Strings), !.
split_left([Head|Tail], Sep, N, Accumulator, [Reversed|Strings]) :-
    member(Head, Sep),
    reverse(Accumulator, Reversed),
    % Force early evaluation
    NSub is N - 1,
    split_left(Tail, Sep, NSub, [], Strings), !.
split_left([Head|Tail], Sep, N, Accumulator, Strings) :-
    split_left(Tail, Sep, N, [Head|Accumulator], Strings), !.

assist("define") :- 
    write("Defines a function from user input."), nl,
    write("Format: define `fnName` arg1, arg2 :: output1, output2 | doc "), nl, !.
assist("clear") :- 
    write("Clears the database of functions."), nl,
    write("Format: clear"), nl, !.
assist(String) :- 
    write("Unrecognized command ~"),
    write(String), nl,
    write("Available commands: define, clear"), nl, !.

execute_command(String) :-
    split_left(String, " ", 2, ["define", FnName, Rest]),
    write("fnName is "),
    write(FnName), nl, 
    write(Rest), nl,
    split_left(Rest, " :", 1, [Inputs, OutputDoc]),
    write("Inputs is "),
    write(Inputs), nl, 
    split_string(Inputs, ", ", ", ", InputTypes),
    split_left(OutputDoc, "| ", 1, [Outputs, Docs]),
    write("Outputs is "),
    write(Outputs), nl,
    split_string(Outputs, ", ", ", ", OutputTypes),
    write("OutputTypes is "),
    write(OutputTypes), nl,
    assertz(function(FnName, InputTypes, OutputTypes, Docs)), !.

execute_command("clear") :-
    retractall(function(_, _, _, _)),
    write("All functions have been erased."), nl, !.

execute_command("help") :-
    write("Available commands: define, clear"), nl,
    write("Use help command for help with a particular command"), nl, !.

execute_command(String) :-
    split_left(String, " ", 2, ["help", Command]),
    assist(Command), !.

input_loop() :-
    write("Enter a command."), nl,
    read(Command),
    execute_command(Command),
    input_loop().

main(_Argv) :- input_loop().

