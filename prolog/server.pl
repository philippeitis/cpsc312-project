:- module(server, [server/1, shutdown/1, server/2]).

%% Load this file in prolog, and use server(Port). to launch it.

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_files)).

:- use_module(function).
:- use_module(function/parse).
:- use_module(function/serde).
:- use_module(constraints, [and_constraint/5, no_constraint/3]).
:- use_module(string_constraints, [add_string_constraint/5]).
:- use_module(func_constraints).
:- use_module(path_constraints).
:- use_module(search).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Register handlers
:- http_handler(
    root(openapi),
    openapi,
    [method(get)]
).

:- http_handler(
    root(.),
    http_reply_from_files('./web_content', []),
    [prefix]
).

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

:- http_handler(
    root(path),
    path_endpoint,
    [method(get)]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Starting and stopping server
suppress_start_msg :- 
    getenv('IN_DOCKER', '1').

server(Port, silent) :-	http_server(http_dispatch, [port(Port), silent(true)]), !.
server(Port) :-	suppress_start_msg,
    http_server(http_dispatch, [port(Port), silent(true)]), !.
server(Port) :-	\+suppress_start_msg,
    http_server(http_dispatch, [port(Port), silent(true)]),
    ansi_format([fg(blue)], 'Started server at ', []),
    ansi_format([bold, fg(blue)], 'http://localhost:~w/~n', [Port]),
    ansi_format([fg(blue)], 'Go to ', []),
    ansi_format([bold, fg(blue)], 'http://localhost:~w/openapi', [Port]),
    ansi_format([fg(blue)], ' to interact with the OpenAPI specification~n', []),
    ansi_format([fg(blue)], 'Go to ', []),
    ansi_format([bold, fg(blue)], 'http://localhost:~w/main.html', [Port]),
    ansi_format([fg(blue)], ' to interact with the application~n', []).

shutdown(Port) :- http_stop_server(Port, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Hide unconstrained variables.
render_param(Param, "?") :- var(Param), !.
render_param(Param, Param) :- !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Defining, searching, and deleting functions

%% Finds a single function with the constraints and prints it.
find_and_fmt_func(Constraint) :-
    find_items(Constraint, [Uuid|Uuids]),
    maplist(get_function, [Uuid|Uuids], Funcs),
    jsonify_funcs(Funcs, JsonFuncs),
    reply_json_dict(_{msg:"Found functions", functions: JsonFuncs}), !.

find_and_fmt_func(_) :-
    format('Status: 404~n'), % Not found
    format('Content-type: application/json~n~n'),
    json_write_dict(current_output, _{msg:"Functions matching query not found"}), !.

%% Helper for correctly constraining lists.
nonempty_list([], true, []) :- !.
nonempty_list([], false, _) :- !.
nonempty_list(NonEmpty, _, NonEmpty) :- !.

%% Parses parameters for search requests.
% Can be partially specified.

parse_func_search_request(Request, Constraint) :-
    Choices = [re, eq, lev, substr, subseq, fsubstr, sim, subsim],
    http_parameters(Request,
        [
            name(Name, [string, default(none)]),
            name_cmp(NameCmp, [default(lev), oneof(Choices)]),
            docs(Docs, [string, default(none)]),
            doc_cmp(DocCmp, [default(substr), oneof(Choices)]),
            uuid(Uuid, [atom, default(none)]),
            uuid_cmp(UuidCmp, [default(eq), oneof(Choices)]),
            inputs(Inputs0, [list(string)]),
            no_inputs(NoInputs, [boolean, default(false)]),
            outputs(Outputs0, [list(string)]),
            no_outputs(NoOutputs, [boolean, default(false)])
        ]),
    nonempty_list(Inputs0, NoInputs, Inputs),
    nonempty_list(Outputs0, NoOutputs, Outputs),
    add_string_constraint(function:func_field(name), Name, NameCmp, no_constraint, C0),
    add_string_constraint(function:func_field(docs), Docs, DocCmp, C0, C1),
    add_string_constraint(function:func_field(uuid), Uuid, UuidCmp, C1, C2),
    Constraint = constraints:and_constraint(
        func_constraints:input_constraint(Inputs),
        constraints:and_constraint(func_constraints:output_constraint(Outputs), C2)
    ).

attempt_fn_deletion(Uuid) :-
    specialized(Parent, Uuid),
    format('Status: 405~n'), % HTTP not allowed
    format('Content-type: application/json~n~n'),
    json_write_dict(current_output,
        _{
            msg:"Can not remove specialization - try deleting parent uuid",
            parent_uuid:Parent
        }
    ), !.

attempt_fn_deletion(Uuid) :-
    fname(Uuid, _),
    retractall(function(Uuid, _, _, _, _, _)),
    findall(
        Child,
        (
            specialized(Uuid, Child),
            retractall(specialized(Uuid, Child)),
            retractall(function(Child, _, _, _, _, _))
        ),
        Children
    ),
    reply_json_dict(_{msg: "Removed", uuids:[Uuid|Children]}), !.

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
    parse_func_search_request(Request, Constraint),
    find_and_fmt_func(Constraint).

fn_endpoint(post, Request) :-
    http_read_json(Request, JsonIn, [json_object(dict)]),
    jsonify_funcs([Fn], [JsonIn]),
    Fn = function(Uuid, Name, Generics, Inputs, Outputs, Docs),
    add_function(Uuid, Name, Generics, Inputs, Outputs, Docs),
    format(string(Msg), "Created func ~w", [Name]),
    reply_json_dict(_{msg: Msg, uuid:Uuid}).

fn_endpoint(delete, Request) :-
    http_parameters(Request, [
        uuid(Uuid, [atom])
    ]),
    attempt_fn_deletion(Uuid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Defining, searching, and deleting types

parse_type_search_request(Request, C2) :-
    Choices = [re, eq, lev, substr, subseq, fsubstr, sim, subsim],
    http_parameters(Request, [
            name(Name, [string, default(none)]),
            name_cmp(NameCmp, [default(lev), oneof(Choices)]),
            docs(Docs, [string, default(none)]),
            doc_cmp(DocCmp, [default(substr), oneof(Choices)]),
            uuid(Uuid, [atom, default(none)]),
            uuid_cmp(UuidCmp, [default(eq), oneof(Choices)])
    ]),
    add_string_constraint(type_field(name), Name, NameCmp, no_constraint, C0),
    add_string_constraint(type_field(docs), Docs, DocCmp, C0, C1),
    add_string_constraint(type_field(uuid), Uuid, UuidCmp, C1, C2).

find_and_fmt_types(Constraint) :-
    find_items(Constraint, [Uuid|Uuids]),
    maplist(get_type, [Uuid|Uuids], Types),
    jsonify_types(Types, JsonTypes),
    reply_json_dict(_{msg:"Found types", types: JsonTypes}), !.

find_and_fmt_types(_) :-
    format('Status: 404~n'), % Not found
    format('Content-type: application/json~n~n'),
    json_write_dict(current_output, _{msg:"Types matching query not found"}), !.

attempt_type_deletion(Uuid) :-
    type(Uuid, Name, Generics, Impls, Docs),
    retractall(type(Uuid, Name, Generics, Impls, Docs)),
    reply_json_dict(_{msg: "Removed type", uuid:Uuid}), !.

attempt_type_deletion(Uuid) :-
    format('Status: 404~n'), % Not found
    format('Content-type: application/json~n~n'),
    json_write_dict(current_output,
        _{
            msg:"Uuid not found",
            uuid:Uuid
        }
    ).

type_endpoint(get, Request) :-
    parse_type_search_request(Request, Constraint), 
    find_and_fmt_types(Constraint).

type_endpoint(post, Request) :-
    http_read_json(Request, JsonIn, [json_object(dict)]),
    jsonify_type(Ty, JsonIn),
    Ty = type(Uuid, Name, Generics, Impls, Docs),
    add_type(Uuid, Name, Generics, Impls, Docs),
    format(string(Msg), "Created type ~w", [Name]),
    reply_json_dict(_{msg: Msg, uuid:Uuid}), !.

type_endpoint(post, _) :-
    format('Status: 400~n'), % User error
    format('Content-type: application/json~n~n'),
    json_write_dict(current_output, _{msg: "Malformed input"}).

type_endpoint(delete, Request) :-
    http_parameters(
        Request,
        [uuid(Uuid, [atom])]
    ),
    attempt_type_deletion(Uuid).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% OpenAPI endpoint
%% favicon generated using https://redketchup.io/favicon-generator
openapi(_) :-
    reply_html_page(
        \['<!doctype html>\n'],
        [
            title('FastFunc OpenAPI'),
            \['<meta charset="utf-8">'],
            script(
                [
                    type='module',
                    src='https://unpkg.com/rapidoc/dist/rapidoc-min.js'
                ],
                []
            ),
            \['<link rel="apple-touch-icon" sizes="180x180" href="/apple-touch-icon.png">'],
            \['<link rel="icon" type="image/png" sizes="32x32" href="/favicon-32x32.png">'],
            \['<link rel="icon" type="image/png" sizes="16x16" href="/favicon-16x16.png">'],
            \['<link rel="manifest" href="/site.webmanifest">']
        ],
        [
            \['<rapi-doc spec-url="./openapi.json"></rapi-doc>']
        ]
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Path endpoint

add_cycle_constraint(false, List, List).
add_cycle_constraint(true, List, [path_constraints:cycle_constraint|List]).

parse_path_search_request(Request, Strategy, FnConstraint, PathConstraint) :-
    Choices = [re, eq, lev, substr, subseq, fsubstr, sim, subsim],
    http_parameters(Request,
        [
            name(Name, [string, default(none)]),
            name_cmp(NameCmp, [default(lev), oneof(Choices)]),
            docs(Docs, [string, default(none)]),
            doc_cmp(DocCmp, [default(substr), oneof(Choices)]),
            uuid(Uuid, [atom, default(none)]),
            uuid_cmp(UuidCmp, [default(eq), oneof(Choices)]),
            inputs(Inputs0, [list(string)]),
            no_inputs(NoInputs, [boolean, default(false)]),
            outputs(Outputs0, [list(string)]),
            no_outputs(NoOutputs, [boolean, default(false)]),
            path_length(Length, [integer, default(999)]),
            strategy(Strategy, [oneof(dfs, bfs, bestfs), default(dfs)]),
            cycles(Cycles, [boolean, default(false)])
            % this would be cool to have if we could figure out a good way to do pagination.
            % num_paths(Num, [integer, default(25)])
        ]),
    nonempty_list(Inputs0, NoInputs, Inputs),
    nonempty_list(Outputs0, NoOutputs, Outputs),
    add_string_constraint(function:func_field(name), Name, NameCmp, no_constraint, C0),
    add_string_constraint(function:func_field(docs), Docs, DocCmp, C0, C1),
    add_string_constraint(function:func_field(uuid), Uuid, UuidCmp, C1, C2),
    FnConstraint = constraints:and_constraint(
        func_constraints:input_constraint(Inputs),
        C2
    ),
    add_cycle_constraint(Cycles, [
        path_constraints:output_constraint(Outputs),
        path_constraints:length_constraint(Length)
    ], PathConstraints),
    PathConstraint = path_constraints:and_constraint(PathConstraints).

find_and_fmt_paths(Strategy, FnConstraint, PathConstraint) :-
    setof(
        Path,
        func_path_init(Strategy, FnConstraint, PathConstraint, Path),
        Paths
    ),
    flatten(Paths, AllFnUuids),
    sort(AllFnUuids, FnUuids),
    maplist(get_function, FnUuids, Funcs),
    jsonify_funcs(Funcs, JsonFuncs),
    length(Paths, PLen),
    length(Funcs, FLen),
    PLen >= 1,
    format(string(Msg), "Found ~w paths (~w functions)", [PLen, FLen]),
    reply_json_dict(_{msg:Msg, paths: Paths, functions:JsonFuncs}), !.

find_and_fmt_paths(_) :-
    format('Status: 404~n'), % Not found
    format('Content-type: application/json~n~n'),
    json_write_dict(current_output, _{msg:"Paths matching query not found"}), !.


path_endpoint(Request) :-
    format(user_output, "~w~n", [[Request]]),
    parse_path_search_request(Request, Strategy, FnConstraint, PathConstraint), 
    find_and_fmt_paths(Strategy, FnConstraint, PathConstraint).

