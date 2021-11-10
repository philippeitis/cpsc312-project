:- module(server, [server/1]).

%% Load this file in prolog, and use server(Port). to launch it.

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_parameters)).

:- use_module(function).
:- use_module(search).

:- http_handler(
    root(func),
    func(Method),
    [method(Method), methods([get, post, delete])]
).

:- http_handler(root(hello_world), say_hi, []).		% (1)

server(Port) :-						% (2)
        http_server(http_dispatch, [port(Port)]).

render_param(Param, "?") :- var(Param), !.
render_param(Param, Param) :- !.

find_and_fmt_func(FuncName, Inputs, Outputs, Docs) :-
    function(FuncName, Inputs, Outputs, Docs),
    format('Found func: ~w ~w :: ~w | ~w~n', [FuncName, Inputs, Outputs, Docs]), !.

find_and_fmt_func(FuncName0, Inputs0, Outputs0, Docs0) :-
    render_param(FuncName0, FuncName),
    render_param(Inputs0, Inputs),
    render_param(Outputs0, Outputs),
    render_param(Docs0, Docs),
    format('No matching func found: ~w ~w :: ~w | ~w~n', [FuncName, Inputs, Outputs, Docs]), !.

nonempty_list([], true, []) :- !.
nonempty_list([], false, _) :- !.
nonempty_list(_, _, _) :- !.

parse_func_request_search(Request, FuncName, Inputs, Outputs, Docs) :-
    http_parameters(Request,
        [
            name(FuncName, [string, default(none)]),
            no_inputs(NoInputs, [boolean, default(false)]),
            no_outputs(NoOutputs, [boolean, default(false)]),
            inputs(Inputs0, [list(string)]),
            outputs(Outputs0, [list(string)]),
            docs(Docs, [string, optional(true)])
        ]),
    nonempty_list(Inputs0, NoInputs, Inputs),
    nonempty_list(Outputs0, NoOutputs, Outputs).

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

func(get, Request) :-					% (3)
    parse_func_request_search(Request, FuncName, Inputs, Outputs, Docs),
    format('Content-type: text/plain~n~n'),
    find_and_fmt_func(FuncName, Inputs, Outputs, Docs).

func(post, Request) :-					% (3)
    parse_func_request_insert(Request, FuncName, Inputs, Outputs, Docs),
    assertz(function(FuncName, Inputs, Outputs, Docs)),
    format('Content-type: text/plain~n~n'),
    format('Created func ~w~n', [FuncName]).

func(delete, Request) :-					% (3)
    parse_func_request_search(Request, FuncName, Inputs, Outputs, Docs),
    retractall(function(FuncName, Inputs, Outputs, Docs)),
    format('Content-type: text/plain~n~n'),
    format('Removed func ~w~n', [FuncName]).