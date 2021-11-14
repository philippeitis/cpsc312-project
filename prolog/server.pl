:- module(server, [server/1]).

%% Load this file in prolog, and use server(Port). to launch it.

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_parameters)).

:- use_module(function).
:- use_module(function/parse).
:- use_module(func_constraints).
:- use_module(search).

%% Initialize server
:- http_handler(
    root(func),
    func(Method),
    [method(Method), methods([get, post, delete])]
).

server(Port) :-	http_server(http_dispatch, [port(Port)]).

%% Hide unconstrained variables.
render_param(Param, "?") :- var(Param), !.
render_param(Param, Param) :- !.

%% Finds a single function with the constraints and prints it.
find_and_fmt_func(FuncName, Inputs, Outputs, Docs, StringCmpName, StringCmpDocs) :-
    func_search(FuncName, Inputs, Outputs, Docs, StringCmpName, StringCmpDocs, [Uuid|_]),
    format_func(String, Uuid),
    format("Found func: ~w~n", [String]).

find_and_fmt_func(FuncName0, Inputs0, Outputs0, Docs0, _, _) :-
    render_param(FuncName0, FuncName),
    render_param(Inputs0, Inputs),
    render_param(Outputs0, Outputs),
    render_param(Docs0, Docs),
    format_skeleton(String, FuncName, [], Inputs, Outputs, Docs),
    format("No matching func found: ~w~n", [String]), !.

%% Helper for correctly constraining lists.
nonempty_list([], true, []) :- !.
nonempty_list([], false, _) :- !.
nonempty_list(NonEmpty, _, NonEmpty) :- !.

%% Parses parameters for search requests.
% Can be partially specified.
parse_func_request_search(Request, FuncName, Inputs, Outputs, Docs, StringCmpName, StringCmpDocs) :-
    http_parameters(Request,
        [
            name(FuncName, [string, default(none)]),
            no_inputs(NoInputs, [boolean, default(false)]),
            no_outputs(NoOutputs, [boolean, default(false)]),
            inputs(Inputs0, [list(string)]),
            outputs(Outputs0, [list(string)]),
            docs(Docs, [string, default(none)]),
            name_cmp(StringCmpName, [default(lev), oneof([re, eq, lev, substr, subseq])]),
            doc_cmp(StringCmpDocs, [default(substr), oneof([re, eq, lev, substr, subseq])])
        ]),
    nonempty_list(Inputs0, NoInputs, Inputs),
    nonempty_list(Outputs0, NoOutputs, Outputs).

%% Parses parameters for insertion of function
% Requires function signature to be defined
parse_func_request_insert(Request, FuncName, Inputs, Outputs, Docs) :-
    http_parameters(Request,
        [
            name(FuncName, [string]),
            no_inputs(NoInputs, [boolean, default(false)]),
            no_outputs(NoOutputs, [boolean, default(false)]),
            inputs(Inputs0, [list(string)]),
            outputs(Outputs0, [list(string)]),
            docs(Docs, [string, default("")])
        ]),
    nonempty_list(Inputs0, NoInputs, Inputs),
    nonempty_list(Outputs0, NoOutputs, Outputs).

%% REST API
func(get, Request) :-
    parse_func_request_search(Request, FuncName, Inputs, Outputs, Docs, StringCmpName, StringCmpDocs),
    format("Content-type: text/plain~n~n"),
    find_and_fmt_func(FuncName, Inputs, Outputs, Docs, StringCmpName, StringCmpDocs).

func(post, Request) :-
    parse_func_request_insert(Request, FuncName, Inputs, Outputs, Docs),
    add_function(_, FuncName, [], Inputs, Outputs, Docs),
    format("Content-type: text/plain~n~n"),
    format("Created func ~w~n", [FuncName]).

%% Lists all functions which are deleted.
func(delete, Request) :-
    parse_func_request_search(Request, FuncName, Inputs, Outputs, Docs, StringCmpName, StringCmpDocs),
    func_search(FuncName, Inputs, Outputs, Docs, StringCmpName, StringCmpDocs, [Uuidx|Uuids]),
    format("Content-type: text/plain~n~n"),
    fname(Uuidx, Namex),
    retract(function(Uuidx, _, _, _, _, _)),
    format("Removed func ~w", [Namex]),
    foreach(
        member(Uuid, Uuids),
        (
            fname(Uuid, Name),
            retractall(function(Uuid, _, _, _, _, _)),
            format(", ~w", [Name])
        )
    ),
    format("~n", []), !.

% Fallthrough when delete fails.
func(delete, _) :-
    format("Content-type: text/plain~n~n"),
    format("No matches found~n", []), !.
