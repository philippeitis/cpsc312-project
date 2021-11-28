:- module(nlp, [similarity/3, sub_similarity/3, setup_tokenizer/0]).
:- use_module(library(http/http_client)).

:- dynamic streams/3.

:- at_halt(close_tokenizer).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions for OS detection
get_os(unix) :-
    current_prolog_flag(unix, true), !.
get_os(apple) :-
    current_prolog_flag(apple, true), !.
get_os(windows) :-
    current_prolog_flag(windows, true), !.

relative_python_path("./venv/bin/python") :-
    get_os(unix).

relative_python_path("./venv/bin/python") :-
    get_os(apple).

relative_python_path("./venv/scripts/python.exe") :-
    get_os(windows).

sys_python('python3') :- get_os(unix).
sys_python('python3') :- get_os(apple).
sys_python('python') :- get_os(windows).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helps with various things
install_spacy :-
    relative_python_path(Python),
    process_create(
        Python,
        ['-m', 'pip', 'install', '-U', 'pip', 'setuptools', 'wheel', 'spacy'],
        [process(PID0), stdout(std), stderr(std)]
    ),
    process_wait(PID0, exit(0)),
    process_create(
        Python,
        ['-m', 'spacy', 'download', 'en_core_web_md'],
        [process(PID1), stdout(std), stderr(std)]
    ),
    process_wait(PID1, exit(0)).

%% Performs the necessary setup steps to create the initial Python environment
% for the tokenizer, provided that ./venv does not exist.
% 
% If the environment is not successfully created, simply delete it and run this
% again.
setup_tokenizer :-
    exists_directory('./venv'),
    format("Virtual environment already exists.~n"), !.

setup_tokenizer :-
    \+exists_directory('./venv'),
    sys_python(Python),
    format("Creating virtual environment...~n"),
    process_create(
        path(Python),
        ['-m', 'venv', './venv'],
        [process(PID), stdout(std), stderr(std)]
    ),
    process_wait(PID, exit(0)),
    format("Created virtual environment.~n"),
    install_spacy,
    format("Installed spaCy.~n"), !.

%% launch_tokenizer(-In:stream, -Out:stream)
% If a tokenizer is currently running, unifies In and Out with the tokenizer's
% input and output streams, respectively. Otherwise, creates a tokenizer instance,
% and unifies the input and output streams with In and Out, respectively.
launch_tokenizer(In, Out) :-
    streams(tokenizer, In, Out), !.

launch_tokenizer(In, Out) :-
    \+streams(tokenizer, _, _),
    relative_python_path(Python),
    process_create(
        Python,
        ['nlp.py'],
        [stdin(pipe(In)), stdout(pipe(Out)), stderr(null)]
    ),
    assertz(streams(tokenizer, In, Out)), !.

%% Closes the tokenizer by writing exit to the input stream.
close_tokenizer :-
    (
        streams(tokenizer, In, _) -> (
            write(In, "exit\n"),
            flush_output(In),
            retractall(streams(tokenizer, _, _))
        ); true
    ).

%% similarity(+Docs:string, +Needle:string, -Similarity:float)
% Unifies Similarity with the similarity between Docs and Needle, as computed
% by spaCy's similarity method.
similarity(Docs, Needle, Similarity) :-
    launch_tokenizer(In, Out),
    string_length(Docs, LenA),
    string_length(Needle, LenB),
    format(In, "similarity ~w ~w~n~w~w", [LenA, LenB, Docs, Needle]),
    flush_output(In),
    read_line_to_string(Out, "OK"),
    read_line_to_string(Out, String),
    number_string(Similarity, String).

%% similarity(+Docs:string, +Needle:string, -Similarity:float)
% Unifies Similarity with the similarity between Docs and Needle, as determined
% by comparing a sliding window of words in Docs to Needle.
sub_similarity(Docs, Needle, Similarity) :-
    launch_tokenizer(In, Out),
    string_length(Docs, LenA),
    string_length(Needle, LenB),
    format(In, "sub_similarity ~w ~w~n~w~w", [LenA, LenB, Docs, Needle]),
    flush_output(In),
    read_line_to_string(Out, "OK"),
    read_line_to_string(Out, String),
    number_string(Similarity, String).
