:- initialization(main, main).

:- use_module(function).
:- use_module(string_op).
:- use_module(search).
:- use_module(server).
:- use_module(library(dcg/basics)).

%% pretty_print_path(List[function])
pretty_print_path([]) :- !.
pretty_print_path([Func]) :-
    write(Func), !.
pretty_print_path([Head|Tail]) :-
    format("~w -> ", [Head]), pretty_print_path(Tail), !.

append(Left, Mid, Right, List) :-
    append(Left, Mid, LeftMid),
    append(LeftMid, Right, List).

parse_sig(Name, Inputs, Outputs, Docs) -->
    string(NameCodes),
    {string_codes(Name, NameCodes)},
    dcol, 
    parse_type_sig(Inputs, Outputs),
    optional_doc(Docs).

parse_type_sig(Inputs, Outputs) -->
    "[", list_of_types(Inputs), "]",
    wh, "->", wh,
    "[", list_of_types(Outputs), "]".

optional_doc(Docs) --> tail, string(DocCodes), {string_codes(Docs, DocCodes)}.
optional_doc("") --> wh.
single_type(Type) --> string(TypeCode), {string_codes(Type, TypeCode)}.

list_of_types([Type|Rem]) --> single_type(Type), wh, ",", wh, list_of_types(Rem).
list_of_types([Type]) --> single_type(Type).
list_of_types([]) --> [].

wh -->  " ", wh.
wh -->  "".
dcol --> wh, "::", wh.
tail --> wh, "|", wh.

parse_signature(String, Name, Inputs, Outputs, Docs) :-
    string_codes(String, Codes),
    phrase(parse_sig(Name, Inputs, Outputs, Docs), Codes).

parse_types(String, Inputs, Outputs) :-
    string_codes(String, Codes),
    phrase(parse_type_sig(Inputs, Outputs), Codes).

command("define").
command("clear").
command("search").
command("path").
command("store").
command("load").
command("launch").
command("quit").

assist("define") :- 
    write("Defines a function from user input."), nl,
    write("Example: define fnName :: [arg1, arg2] -> [output1, output2] | doc "), nl, !.
assist("clear") :- 
    write("Clears the database of functions."), nl,
    write("Example: clear"), nl, !.
assist("search") :- 
    write("Finds a function with the given signature."), nl,
    write("Example: search :: [arg1, arg2] -> [output1, output2]"), nl, !.
assist("path") :- 
    write("Finds a sequence of function which transform the input to the output."), nl,
    write("Example: path :: [arg1, arg2] -> [output1, output2]"), nl, !.
assist("store") :- 
    write("Persists the existing functions to disk at the provided path."), nl,
    write("Example: store ./path/to/file.json"), nl, !.
assist("load") :- 
    write("Loads the persisted functions from disk at the provided path."), nl,
    write("Example: load ./path/to/file.json"), nl, !.
assist("launch") :- 
    write("Launches the server on the given port."), nl,
    write("Example: launch 5000"), nl, !.
assist("quit") :-
    write("Terminates the program."), nl, !.
assist(String) :- 
    format("Unrecognized command ~~~w", String), nl,
    available_commands(), !.

read_y :-
    get_single_char(YCode),
    char_code(Y, YCode),
    member(Y, ['y', 'Y']), !.

assist_nearest(String, Corrected) :-
    findall((Distance, Command),
        (
            command(Command),
            levenshtein_distance(Command, String, Distance)
        ), Commands),
    min_member((Distance, Corrected), Commands),
    format("Did you mean ~w? Type y or n: ", [Corrected]),
    read_y(), !.

available_commands() :-
    write("Available commands: "), nl,
    foreach(command(Name), (format("    ~w", Name), nl)).

execute_command(String) :-
    split_left(String, " ", 1, ["define", Rest]),
    parse_signature(Rest, FnName, InputTypes, OutputTypes, Docs),
    format("Adding function: ~w", [FnName]), nl,
    assertz(function(FnName, InputTypes, OutputTypes, Docs)), !.

execute_command("clear") :-
    retractall(function(_, _, _, _)),
    write("All functions have been erased."), nl, !.

execute_command(String) :-
    split_left(String, " ", 1, ["search", Rest]),
    parse_types(Rest, InputTypes, OutputTypes),
    findnsols(5, Func, func_path_no_cycles(InputTypes, OutputTypes, [Func]), Solns),
    length(Solns, Len),
    format("Found ~w solutions:", [Len]), nl,
    foreach(member(Soln, Solns), (format("Function: ~w", [Soln]), nl)), !.

execute_command(String) :-
    split_left(String, " ", 1, ["path", Rest]),
    parse_types(Rest, InputTypes, OutputTypes),
    findnsols(5, Path, func_path_no_cycles(InputTypes, OutputTypes, Path), Solns),
    length(Solns, Len),
    format("Found ~w solutions:", [Len]), nl,
    foreach(member(Soln, Solns), (pretty_print_path(Soln), nl)), !.

execute_command(String) :-
    split_left(String, " ", 1, ["launch", PortStr]),
    number_string(Port, PortStr),
    catch(
        server(Port),
        error(socket_error(_, 'Address already in use'), _),
        writeln("Port already in use - have you already launched a server?")
    ), !.

execute_command(String) :-
    split_left(String, " ", 1, ["store", Path]),
    open(Path, write, Stream),
    write_json_funcs(Stream),
    close(Stream), !.

execute_command(String) :-
    split_left(String, " ", 1, ["load", Path]),
    open(Path, read, Stream),
    read_json_funcs(Stream),
    close(Stream), !.

execute_command("quit") :- halt(0).

execute_command("help") :-
    write("Use `help command` for help with a particular command"), nl,
    available_commands(), !.

execute_command(String) :-
    split_left(String, " ", 1, ["help", Command]),
    assist(Command), !.

execute_command(String) :-
    split_left(String, " ", 1, [Command|Rest]),
    assist_nearest(Command, Nearest),
    join([Nearest|Rest], " ", CorrectedCommand),
    write(CorrectedCommand), nl,
    execute_command(CorrectedCommand), !.

execute_command(String) :- assist(String), !.

input_loop() :-
    write("Enter a command."), nl,
    read_line_to_string(current_input, Command), nl,
    execute_command(Command),
    input_loop().

main(['--help']) :- execute_command("help"), !.
main(['--help', CommandAtom]) :-
    atom_string(CommandAtom, Command),
    assist(Command), !.
main([CommandAtom|AtomArgs]) :-
    atom_string(CommandAtom, Command),
    findall(String, (
        member(Atom, AtomArgs),
        atom_string(Atom, String)
    ), Args),
    join([Command|Args], " ", CommandStr),
    execute_command(CommandStr),
    input_loop(), !.

main(_Argv) :- input_loop().