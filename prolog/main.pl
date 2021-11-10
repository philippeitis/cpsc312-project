:- use_module(search).
:- use_module(string_op).

pretty_print_path([]) :- !.
pretty_print_path([Func]) :-
    write(Func), !.
pretty_print_path([Head|Tail]) :-
    write(Head), write(" -> "), pretty_print_path(Tail), !.
    
parse_definition(Definition, InputTypes, OutputTypes, Docs) :- 
    split_left(Definition, " :", 1, [Inputs, OutputDoc]),
    split_string(Inputs, ", ", ", ", InputTypes),
    write("InputTypes is "),
    write(InputTypes), nl, 
    split_left(OutputDoc, "| ", 1, [Outputs, Docs]),
    split_string(Outputs, ", ", ", ", OutputTypes),
    write("OutputTypes is "),
    write(OutputTypes), nl.

command("define").
command("clear").
command("search").
command("path").

assist("define") :- 
    write("Defines a function from user input."), nl,
    write("Format: define `fnName` arg1, arg2 :: output1, output2 | doc "), nl, !.
assist("clear") :- 
    write("Clears the database of functions."), nl,
    write("Format: clear"), nl, !.
assist("search") :- 
    write("Finds a function with the given signature."), nl,
    write("Format: search arg1, arg2 :: output1, output2 | doc"), nl, !.
assist("path") :- 
    write("Finds a sequence of function which transform the input to the output."), nl,
    write("Format: search arg1, arg2 :: output1, output2 | doc"), nl, !.
assist(String) :- 
    write("Unrecognized command ~"),
    write(String), nl,
    write("Available commands: define, clear"), nl, !.

available_commands() :-
    write("Available commands: "), nl,
    foreach(command(Name), (write("    "), write(Name), nl)).

execute_command(String) :-
    split_left(String, " ", 2, ["define", FnName, Rest]),
    write("fnName is "),
    write(FnName), nl, 
    write(Rest), nl,
    parse_definition(Rest, InputTypes, OutputTypes, Docs),
    assertz(function(FnName, InputTypes, OutputTypes, Docs)), !.

execute_command("clear") :-
    retractall(function(_, _, _, _)),
    write("All functions have been erased."), nl, !.

execute_command(String) :-
    split_left(String, " ", 1, ["search", Rest]),
    parse_definition(Rest, InputTypes, OutputTypes, Docs),
    findnsols(5, Func, funcPathNoCycles(InputTypes, OutputTypes, [Func]), Solns),
    length(Solns, Len),
    write("Found "), write(Len), write(" solutions:"), nl,
    foreach(member(Soln, Solns), (write("Function: "), write(Soln), nl)), !.

execute_command(String) :-
    split_left(String, " ", 1, ["path", Rest]),
    parse_definition(Rest, InputTypes, OutputTypes, Docs),
    findnsols(5, Path, funcPathNoCycles(InputTypes, OutputTypes, Path), Solns),
    length(Solns, Len),
    write("Found "), write(Len), write(" solutions:"), nl,
    foreach(member(Soln, Solns), (pretty_print_path(Soln), nl)), !.

execute_command("help") :-
    available_commands(),
    write("Use `help command` for help with a particular command"), nl, !.

execute_command(String) :-
    split_left(String, " ", 2, ["help", Command]),
    assist(Command), !.

input_loop() :-
    write("Enter a command."), nl,
    read(Command),
    execute_command(Command),
    input_loop().

main(_Argv) :- input_loop().