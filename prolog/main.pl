:- use_module(function).
:- use_module(string_op).
:- use_module(search).

%% pretty_print_path(List[function])
pretty_print_path([]) :- !.
pretty_print_path([Func]) :-
    write(Func), !.
pretty_print_path([Head|Tail]) :-
    format("~w -> ", [Head]), pretty_print_path(Tail), !.

%% default_docs(OutputDoc, Outputs, Docs)
% True if Outputs is the first item of OutputDoc,
% and Docs is either "" and OutputDoc only contains Output,
% or Docs is the second item of Output.
default_docs([Outputs], Outputs, "").
default_docs([Outputs, Docs], Outputs, Docs).

parse_definition(Definition, InputTypes, OutputTypes, Docs) :- 
    split_left(Definition, " :", 1, [Inputs, OutputDoc]),
    split_string(Inputs, ", ", ", ", InputTypes),
    format("InputTypes is ~w", [InputTypes]), nl,
    split_left(OutputDoc, "| ", 1, OutputDocs),
    default_docs(OutputDocs, Outputs, Docs),
    split_string(Outputs, ", ", ", ", OutputTypes),
    format("OutputTypes is ~w", [OutputTypes]), nl.

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
    write("Format: search arg1, arg2 :: output1, output2"), nl, !.
assist("path") :- 
    write("Finds a sequence of function which transform the input to the output."), nl,
    write("Format: search arg1, arg2 :: output1, output2"), nl, !.
assist(String) :- 
    format("Unrecognized command ~~~w", String), nl,
    available_commands(), !.

available_commands() :-
    write("Available commands: "), nl,
    foreach(command(Name), (format("    ~w", Name), nl)).

execute_command(String) :-
    split_left(String, " ", 2, ["define", FnName, Rest]),
    format("fnName is ~w", [FnName]), nl,
    write(Rest), nl,
    parse_definition(Rest, InputTypes, OutputTypes, Docs),
    assertz(function(FnName, InputTypes, OutputTypes, Docs)), !.

execute_command("clear") :-
    retractall(function(_, _, _, _)),
    write("All functions have been erased."), nl, !.

execute_command(String) :-
    split_left(String, " ", 1, ["search", Rest]),
    parse_definition(Rest, InputTypes, OutputTypes, Docs),
    findnsols(5, Func, func_path_no_cycles(InputTypes, OutputTypes, [Func]), Solns),
    length(Solns, Len),
    format("Found ~w solutions:", [Len]), nl,
    foreach(member(Soln, Solns), (format("Function: ~w", [Soln]), nl)), !.

execute_command(String) :-
    split_left(String, " ", 1, ["path", Rest]),
    parse_definition(Rest, InputTypes, OutputTypes, Docs),
    findnsols(5, Path, func_path_no_cycles(InputTypes, OutputTypes, Path), Solns),
    length(Solns, Len),
    format("Found ~w solutions:", [Len]), nl,
    foreach(member(Soln, Solns), (pretty_print_path(Soln), nl)), !.

execute_command("help") :-
    write("Use `help command` for help with a particular command"), nl,
    available_commands(), !.

execute_command(String) :-
    split_left(String, " ", 2, ["help", Command]),
    assist(Command), !.

execute_command(String) :- assist(String), !.

input_loop() :-
    write("Enter a command."), nl,
    read(Command), nl,
    execute_command(Command),
    input_loop().

main(_Argv) :- input_loop().