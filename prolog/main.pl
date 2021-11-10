:- use_module(search).
:- use_module(string_op).

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

