:- module(nlp, [similarity/3]).
:- use_module(library(http/http_client)).

:- dynamic nlp_streams/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Helper functions for OS detection
get_os(unix) :-
    current_prolog_flag(unix, true).
get_os(apple) :-
    current_prolog_flag(apple, true).
get_os(windows) :-
    current_prolog_flag(windows, true).

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

launch_tokenizer(In, Out) :-
    nlp_streams(In, Out), !.

launch_tokenizer(In, Out) :-
    \+nlp_streams(_, _),
    setup_tokenizer,
    relative_python_path(Python),
    process_create(
        Python,
        ['nlp.py'],
        [stdin(pipe(In)), stdout(pipe(Out)), stderr(null)]
    ),
    assertz(nlp_streams(In, Out)), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
similarity(Docs, Needle, Similarity) :-
    launch_tokenizer(In, Out),
    string_length(Docs, LenA),
    string_length(Needle, LenB),
    format(In, "similarity ~w ~w~n~w~w", [LenA, LenB, Docs, Needle]),
    flush_output(In),
    read_line_to_string(Out, "OK"),
    read_line_to_string(Out, String),
    number_string(Similarity, String).
