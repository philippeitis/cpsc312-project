:- initialization(main, main).

:- use_module(function).
:- use_module(string_op).
:- use_module(search).
:- use_module(server).
:- use_module(library(dcg/basics)).

%% pretty_print_path(List[function])
pretty_print_path([]) :- !.
pretty_print_path([Func]) :-
    fname(Func, Name),
    write(Name), !.
pretty_print_path([Func|Tail]) :-
    fname(Func, Name),
    format("~w -> ", [Name]), pretty_print_path(Tail), !.

wh -->  " ", wh.
wh -->  "".

%% Find the first key/value pair with the corresponding Key,
% returning Default if no such key/value pair exists.
get_key_or_default(List, Key, _, Value) :-
    member((Key, Value), List), !.
get_key_or_default(List, Key, Default, Default) :-
    \+member((Key, _), List), !.

%% Parses options in --key=val format.
parse_options(String, Options) :-
    string_codes(String, Codes),
    phrase((wh, options(Options)), Codes), !.

%% DCG option parsing
options([(Key, Value)|Rem]) -->
    single_option(Key, Value), wh, options(Rem).
options([(Key, Value)]) --> single_option(Key, Value), wh.
options([]) --> [].

single_option(Key, Value) -->
    "--", string(KeyCodes), wh,
    {string_codes(Key, KeyCodes)},
    "=", wh, "\"", escaped_string(ValueCodes), "\"",
    {string_codes(Value, ValueCodes)}.

single_option(Key, Value) -->
    "--", string(KeyCodes), wh,
    {string_codes(Key, KeyCodes)},
    "=", wh, string(ValueCodes),
    {string_codes(Value, ValueCodes)}.

single_option(Key, "") -->
    "--", string(KeyCodes),
    {string_codes(Key, KeyCodes)}.

%% DCG for escaping quote and backslach for regex strings.
escaped_char('"') -->
    "\\\"".
escaped_char('\\') -->
    "\\\\".
escaped_char(C) --> [H], {char_code(C, H)}.

escaped_string([]) --> [].
escaped_string([C]) --> escaped_char(C).
escaped_string([C|Rem]) --> escaped_char(C), escaped_string(Rem).

%% Listing of available commands.
command("define").
command("clear").
command("search").
command("path").
command("store").
command("load").
command("launch").
command("quit").

%% Help strings for available commands.
assist("define") :- 
    write("Defines a function from user input."), nl,
    write("Example: define fnName :: [arg1, arg2] -> [output1, output2] | doc "), nl, !.
assist("clear") :- 
    write("Clears the database of functions."), nl,
    write("Example: clear"), nl, !.
assist("search") :- 
    write("Finds a function with the given signature."), nl,
    write("Example: search :: [arg1, arg2] -> [output1, output2] --name=fnname --name_cmp=eq"), nl, !.
assist("path") :- 
    write("Finds a sequence of functions which transform the input to the output."), nl,
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
assist("os") :-
    write("Prints the currently running OS."), nl, !.

assist(String) :- 
    format("Unrecognized command ~~~w", String), nl,
    available_commands(), !.

%% Succeeds if a single y is read.
read_y :-
    get_single_char(YCode),
    char_code(Y, YCode),
    member(Y, ['y', 'Y']), !.

%% Finds the nearest command to the user input
assist_nearest(String, Corrected) :-
    findall((Distance, Command),
        (
            command(Command),
            levenshtein_distance(Command, String, Distance)
        ), Commands),
    min_member((Distance, Corrected), Commands),
    format("Did you mean ~w? Type y or n: ", [Corrected]),
    read_y(), !.

%% Lists all available commands
available_commands() :-
    write("Available commands: "), nl,
    foreach(command(Name), (format("    ~w", Name), nl)).

%% Executes the user input - assist has examples of usage.
execute_command(String) :-
    split_left(String, " ", 1, ["define", Rest]),
    parse_signature(Rest, FnName, Generics, InputTypes, OutputTypes, Docs),
    format("Adding function: ~w", [FnName]), nl,
    add_function(_, FnName, Generics, InputTypes, OutputTypes, Docs), !.

execute_command("clear") :-
    clear_funcs,
    write("All functions have been erased."), nl, !.

execute_command(String) :-
    split_left(String, " ", 1, ["search", Rest]),
    parse_types(Rest, InputTypes, OutputTypes, OptionStr),
    parse_options(OptionStr, Options),
    get_key_or_default(Options, "name", none, Name),
    get_key_or_default(Options, "docs", none, Docs),
    get_key_or_default(Options, "name_cmp", lev, NameCmp),
    get_key_or_default(Options, "doc_cmp", substr, DocCmp),
    func_search(Name, InputTypes, OutputTypes, Docs, NameCmp, DocCmp, Funcs),
    findnsols(5, FName, (member(Func, Funcs), fname(Func, FName)), Solns),
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
    write_json_metadata(Stream),
    close(Stream), !.

execute_command(String) :-
    split_left(String, " ", 1, ["load", Path]),
    open(Path, read, Stream),
    read_json_metadata(Stream),
    close(Stream), !.

execute_command("quit") :- halt(0).

execute_command("os") :-
    current_prolog_flag(unix, true),
    format("Unix~n"), !.

execute_command("os") :-
    current_prolog_flag(apple, true),
    format("MacOS~n"), !.

execute_command("os") :-
    current_prolog_flag(windows, true),
    format("Windows~n"), !.

execute_command("os") :-
    format("Unknown~n"), !.

execute_command("help") :-
    write("Use `help command` for help with a particular command"), nl,
    available_commands(), !.

execute_command(String) :-
    split_left(String, " ", 1, ["help", Command]),
    assist(Command), !.

execute_command(String) :-
    split_left(String, " ", 1, [Command|Rest]),
    % If this part is correct, command was not
    % sucessfully executed.
    \+command(Command),
    assist_nearest(Command, Nearest),
    join([Nearest|Rest], " ", CorrectedCommand),
    write(CorrectedCommand), nl,
    execute_command(CorrectedCommand), !.

execute_command(String) :- assist(String), !.

%% Core event loop.
input_loop() :-
    write(">>> "),
    read_line_to_string(current_input, Command),
    execute_command(Command),
    input_loop().

%% Intercept any arguments when called from cli.
main(['--help']) :- execute_command("help"), !.
main(['--help', CommandAtom]) :-
    atom_string(CommandAtom, Command),
    assist(Command), !.
main(['--quit'|Command]) :-
    findall(String, (
        member(Atom, Command),
        atom_string(Atom, String)
    ), Args),
    join(Args, " ", CommandStr),
    execute_command(CommandStr), !.
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