:- module(server, [server/1, shutdown/1]).

%% Load this file in prolog, and use server(Port). to launch it.

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).

:- use_module(function).
:- use_module(function/parse).
:- use_module(function/serde).
:- use_module(func_constraints).
:- use_module(search).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Register handlers
:- http_handler(
    root(func),
    fn_endpoint(Method),
    [method(Method), methods([get, post, delete])]
).

:- http_handler(
    root(type),
    type_endpoint(Method),
    [method(Method), methods([get, post, delete])]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Starting and stopping server
server(Port) :-	http_server(http_dispatch, [port(Port)]).
shutdown(Port) :- http_stop_server(Port, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Hide unconstrained variables.
render_param(Param, "?") :- var(Param), !.
render_param(Param, Param) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Defining, searching, and deleting functions

uuids_to_funcs([], []).
uuids_to_funcs([Uuid|Uuids], [Func|Funcs]) :-
    get_function(Uuid, Func),
    uuids_to_funcs(Uuids, Funcs).

%% Finds a single function with the constraints and prints it.
find_and_fmt_func(FuncName, Inputs, Outputs, Docs, StringCmpName, StringCmpDocs) :-
    func_search(FuncName, Inputs, Outputs, Docs, StringCmpName, StringCmpDocs, [Uuid|Uuids]),
    uuids_to_funcs([Uuid|Uuids], Funcs),
    jsonify_funcs(Funcs, JsonFuncs),
    reply_json_dict(_{msg:"Found functions", functions: JsonFuncs}).

find_and_fmt_func(FuncName0, Inputs0, Outputs0, Docs0, _, _) :-
    render_param(FuncName0, FuncName),
    render_param(Inputs0, Inputs),
    render_param(Outputs0, Outputs),
    render_param(Docs0, Docs),
    format_skeleton(String, FuncName, [], Inputs, Outputs, Docs),
    format(string(Msg), "No matching func found: ~w", [String]),
    format('Status: 404~n'), % Not found
    format('Content-type: application/json~n~n'),
    json_write_dict(current_output, _{msg:Msg}), !.

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

attempt_fn_deletion(Uuid) :-
    uuid(Uuid, Uuid),
    specialized(Parent, Uuid),
    format('Status: 405~n'), % HTTP not allowed
    format('Content-type: application/json~n~n'),
    json_write_dict(current_output,
        _{
            msg:"Can not remove specialization - try deleting parent uuid",
            parent_uuid:Parent
        }
    ), !.

attempt_fn_deletion(Uuids) :-
    atom_string(Uuid, Uuids),
    function:fname(Uuid, _),
    retractall(function(Uuid, _, _, _, _, _)),
    reply_json_dict(_{msg: "Removed", uuid:Uuid}), !.

attempt_fn_deletion(Uuid) :-
    format('Status: 404~n'), % Not found
    format('Content-type: application/json~n~n'),
    json_write_dict(current_output,
        _{
            msg:"Uuid not found",
            uuid:Uuid
        }
    ).

%% Function endpoint
fn_endpoint(get, Request) :-
    parse_func_request_search(Request, FuncName, Inputs, Outputs, Docs, StringCmpName, StringCmpDocs),
    find_and_fmt_func(FuncName, Inputs, Outputs, Docs, StringCmpName, StringCmpDocs).

fn_endpoint(post, Request) :-
    parse_func_request_insert(Request, FuncName, Inputs, Outputs, Docs),
    add_function(Uuid, FuncName, [], Inputs, Outputs, Docs),
    format(string(Msg), "Created func ~w", [FuncName]),
    reply_json_dict(_{msg: Msg, uuid:Uuid}).

fn_endpoint(delete, Request) :-
    http_parameters(Request, [
        uuid(Uuid, [string])
    ]),
    attempt_fn_deletion(Uuid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Defining, searching, and deleting types

find_and_fmt_type(Name) :-
    type(Name, Generics, Impls),
    jsonify_type(type(Name, Generics, Impls), JsonType),
    reply_json_dict(_{msg:"Found type", type: JsonType}).

find_and_fmt_type(Name) :-
    format(string(Msg), "No type with name ~w found.", [Name]),
    format('Status: 404~n'), % Not found
    format('Content-type: application/json~n~n'),
    json_write_dict(current_output, _{msg:Msg}), !.

attempt_type_deletion(Name) :-
    type(Name, _, _),
    retractall(type(Name, _, _)),
    reply_json_dict(_{msg: "Removed type", name:Name}), !.

attempt_type_deletion(Name) :-
    format('Status: 404~n'), % Not found
    format('Content-type: application/json~n~n'),
    json_write_dict(current_output,
        _{
            msg:"Name not found",
            name:Name
        }
    ).

%% This should be more interesting (eg. allow using cmp methods here)
type_endpoint(get, Request) :-
    http_parameters(Request,
        [name(Name, [string])]
    ),
    find_and_fmt_type(Name).

type_endpoint(post, Request) :-
    http_read_json(Request, JsonIn, [json_object(dict)]),
    jsonify_type(type(Name, Generics, Impls), JsonIn),
    assertz(type(Name, Generics, Impls)),
    format(string(Msg), "Created type ~w", [Name]),
    reply_json_dict(_{msg: Msg}), !.

type_endpoint(post, _) :-
    format('Status: 400~n'), % User error
    format('Content-type: application/json~n~n'),
    reply_json_dict(_{msg: "Malformed input"}).

type_endpoint(delete, Request) :-
    http_parameters(Request,
        [name(Name, [string])]
    ),
    attempt_type_deletion(Name).
