:- module(server, [server/1]).

%% Load this file in prolog, and use server(Port). to launch it.

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_parameters)).

:- use_module(function).
:- use_module(constraint).
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


get_name_constraint(String, exact, (exact_name_constraint, String)).
get_name_constraint(String, lev, (name_distance_constraint, (String, MaxDis))) :- string_length(String, MaxDis).
get_name_constraint(String, substr, (name_substring_constraint, String)).

get_doc_constraint(String, exact, (exact_doc_constraint, String)).
get_doc_constraint(String, lev, (doc_distance_constraint, (String, MaxDis))) :- string_length(String, MaxDis).
get_doc_constraint(String, substr, (doc_substring_constraint, String)).

add_name_constraint(none, _, Constraints, Constraints) :- !.
add_name_constraint(Name, Method, Constraints, [Constraint|Constraints]) :-
    get_name_constraint(Name, Method, Constraint), !.

add_doc_constraint(none, _, Constraints, Constraints) :- !.
add_doc_constraint(Docs, Method, Constraints, [Constraint|Constraints]) :-
    get_doc_constraint(Docs, Method, Constraint), !.

find_and_fmt_func(FuncName, Inputs, Outputs, Docs, StringCmpName, StringCmpDocs) :-
    add_name_constraint(FuncName, StringCmpName, [], C0),
    add_doc_constraint(Docs, StringCmpDocs, C0, C1),
    find_funcs(
        [Name|_],
        [
            (input_constraint, Inputs)
            |[(output_constraint, Outputs)|C1]
        ]
    ),
    function(Name, InputsOut, OutputsOut, DocsOut),
    format('Found func: ~w ~w :: ~w | ~w~n', [Name, InputsOut, OutputsOut, DocsOut]), !.

find_and_fmt_func(FuncName0, Inputs0, Outputs0, Docs0, _, _) :-
    render_param(FuncName0, FuncName),
    render_param(Inputs0, Inputs),
    render_param(Outputs0, Outputs),
    render_param(Docs0, Docs),
    format('No matching func found: ~w ~w :: ~w | ~w~n', [FuncName, Inputs, Outputs, Docs]), !.

nonempty_list([], true, []) :- !.
nonempty_list([], false, _) :- !.
nonempty_list(NonEmpty, _, NonEmpty) :- !.

parse_func_request_search(Request, FuncName, Inputs, Outputs, Docs, StringCmpName, StringCmpDocs) :-
    http_parameters(Request,
        [
            name(FuncName, [string, default(none)]),
            no_inputs(NoInputs, [boolean, default(false)]),
            no_outputs(NoOutputs, [boolean, default(false)]),
            inputs(Inputs0, [list(string)]),
            outputs(Outputs0, [list(string)]),
            docs(Docs, [string, default(none)]),
            string_cmp_name(StringCmpName, [default(lev), oneof([exact, lev, substr, subseq])]),
            string_cmp_docs(StringCmpDocs, [default(substr), oneof([exact, lev, substr, subseq])])
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
    parse_func_request_search(Request, FuncName, Inputs, Outputs, Docs, StringCmpName, StringCmpDocs),
    format('Content-type: text/plain~n~n'),
    find_and_fmt_func(FuncName, Inputs, Outputs, Docs, StringCmpName, StringCmpDocs).

func(post, Request) :-					% (3)
    parse_func_request_insert(Request, FuncName, Inputs, Outputs, Docs),
    assertz(function(FuncName, Inputs, Outputs, Docs)),
    format('Content-type: text/plain~n~n'),
    format('Created func ~w~n', [FuncName]).

func(delete, Request) :-					% (3)
    parse_func_request_search(Request, FuncName, Inputs, Outputs, Docs, _, _),
    retractall(function(FuncName, Inputs, Outputs, Docs)),
    format('Content-type: text/plain~n~n'),
    format('Removed func ~w~n', [FuncName]).