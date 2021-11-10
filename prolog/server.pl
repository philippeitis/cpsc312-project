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

say_hi(_Request) :-					% (3)
    format('Content-type: text/plain~n~n'),
    format('Hello World!~n').

find_and_fmt_func(FuncName, Inputs, Outputs, Docs) :-
    function(FuncName, Inputs, Outputs, Docs),
    format('Found func: ~w ~w :: ~w | ~w~n', [FuncName, Inputs, Outputs, Docs]), !.

find_and_fmt_func(FuncName, Inputs, Outputs, Docs) :-
    format('No matching func found: ~w ~w :: ~w | ~w~n', [FuncName, Inputs, Outputs, Docs]), !.

func(get, Request) :-					% (3)
    http_parameters(Request,
        [
            func(FuncName, [string, optional(true)]),
            inputs(Inputs, [list(string), optional(true)]),
            outputs(Ouputs, [list(string), optional(true)]),
            docs(Docs, [string, optional(true)])
        ]),
    format('Content-type: text/plain~n~n'),
    find_and_fmt_func(FuncName, Inputs, Outputs, Docs).

func(post, Request) :-					% (3)
    http_parameters(Request,
        [
            func(FuncName, [string]),
            inputs(Inputs, [list(string)]),
            outputs(Ouputs, [list(string)]),
            docs(Docs, [string, default("")])
        ]),
    assertz(function(FuncName, Inputs, Outputs, Docs)),
    format('Content-type: text/plain~n~n'),
    format('Created func ~w~n', [FuncName]).

func(delete, Request) :-					% (3)
    http_parameters(Request,
        [
            func(FuncName, [string]),
            inputs(Inputs, [list(string)]),
            outputs(Ouputs, [list(string)]),
            docs(Docs, [string, default("")])
        ]),
    retract(function(FuncName, Inputs, Outputs, Docs)),
    format('Content-type: text/plain~n~n'),
    format('Removed func ~w~n', [FuncName]).