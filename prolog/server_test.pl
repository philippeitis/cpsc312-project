:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

:- use_module(compat).
:- use_module(function/serde).

:- if(prolog_version_eight).
:- use_module(server).
:- endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Defining, searching, and deleting functions

%% Checks that posting parseInt55 works correctly
post_fn_ok(Port) :-
    jsonify_funcs([function(_, "parseInt55", [], ["str"], ["int"], "Hello world!")], [JsonFn]),
    atom_json_dict(Fn, JsonFn, [as(atom)]),
    http_post([
            protocol(http),
            host(localhost),
            port(Port),
            path('/func')
        ],
        atom('application/json', Fn),
        _{
            msg:"Created func parseInt55",
            uuid:_
        },
        [json_object(dict)]
    ).

%% Runs a request which gets parseInt55 and unfies the response with Rpely
get_fn(Port, Reply) :-
    http_get([
            protocol(http),
            host(localhost),
            port(Port),
            path('/func?name=pant5&name_cmp=subseq')
        ],
        Reply,
        [json_object(dict)]
    ).

%% Runs a request which deletes parseInt55 and unfies the response with Rpely
delete_fn(Port, Uuid, Reply) :-
    atom_concat('/func?uuid=', Uuid, Path),
    http_delete([
            protocol(http),
            host(localhost),
            port(Port),
            path(Path)
        ],
        Reply,
        [json_object(dict)]
    ).

:- begin_tests('function endpoint tests', [condition(prolog_version_eight)]).

test(
    "Trying to delete nonexistent uuid results in 404",
    [
        setup(server(Port, silent)),
        error(existence_error(_, _)),
        cleanup(shutdown(Port))
    ]) :-
    % Should give status 404
    delete_fn(Port, '0', _{
        msg:"No matches found for provided query."
    }).

test(
    "Trying to get nonexistent function results in 404",
    [
        setup(server(Port, silent)),
        error(existence_error(_, _)),
        cleanup(shutdown(Port))
    ]) :-
    get_fn(Port, _{
        msg:"No matching func found: pant5 :: ? -> ? | none"
    }).

test(
    "Runs an example client session",
    [
        setup(server(Port, silent)),
        nondet,
        cleanup(shutdown(Port))
    ]) :-
    post_fn_ok(Port),
    post_fn_ok(Port),
    get_fn(Port, _{msg:"Found functions", functions:[
        _{
            uuid:Uuid1,
            name:"parseInt55",
            generics:[],
            inputs:["str"],
            outputs:["int"],
            docs:"Hello world!"
        },
        _{
            uuid:Uuid2,
            name:"parseInt55",
            generics:[],
            inputs:["str"],
            outputs:["int"],
            docs:"Hello world!"
        }
    ]}),
    % Delete all copies of parseInt55.
    assertion(
        delete_fn(Port, Uuid1, _{msg: "Removed", uuids:[Uuid1]})
    ),
    assertion(
        delete_fn(Port, Uuid2, _{msg: "Removed", uuids:[Uuid2]})
    ),
    % Check that we did in fact delete parseInt55 - this should be 404
    % If not, we don't throw an exception and read fail.
    catch((get_fn(Port, _), fail), _, true).

:- end_tests('function endpoint tests').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Defining, searching, and deleting types

%% Checks that posting AddSet<X: Add> works correctly
post_type_ok(Port, Uuid) :-
    jsonify_type(type(_, "Set", [generic("X", ["Add"])], ["Add"], ""), JsonType),
    atom_json_dict(Type, JsonType, [as(atom)]),
    http_post([
            protocol(http),
            host(localhost),
            port(Port),
            path('/type')
        ],
        atom('application/json', Type),
        _{
            msg:"Created type Set",
            uuid:Uuids
        },
        [json_object(dict)]
    ),
    atom_string(Uuid, Uuids).

%% Runs a request which gets parseInt55 and unfies the response with Rpely
get_type(Port, Uuid, Reply) :-
    atom_concat('/type?uuid=', Uuid, Path),
    http_get([
            protocol(http),
            host(localhost),
            port(Port),
            path(Path)
        ],
        Reply,
        [json_object(dict)]
    ).

%% Runs a request which deletes parseInt55 and unfies the response with Rpely
delete_type(Port, Uuid, Reply) :-
    atom_concat('/type?uuid=', Uuid, Path),
    http_delete([
            protocol(http),
            host(localhost),
            port(Port),
            path(Path)
        ],
        Reply,
        [json_object(dict)]
    ).

:- begin_tests('type endpoint tests', [condition(prolog_version_eight)]).

test(
    "Trying to delete nonexistent type results in 404",
    [
        setup(server(Port, silent)),
        error(existence_error(_, _)),
        cleanup(shutdown(Port))
    ]) :-
    % Should give status 404
    delete_type(Port, 'not a real type', _{
        msg:"Name not found",
        name:'not a real type'
    }).

test(
    "Trying to get nonexistent function results in 404",
    [
        setup(server(Port, silent)),
        error(existence_error(_, _)),
        cleanup(shutdown(Port))
    ]) :-
    get_type(Port, _{
        msg:"No type with name Set found."
    }).

test(
    "Post succeeds",
    [
        setup(server(Port, silent)),
        nondet,
        cleanup(shutdown(Port))
    ]) :-
    post_type_ok(Port, _).

test(
    "Get succeeds after post",
    [
        setup(server(Port, silent)),
        nondet,
        cleanup(shutdown(Port))
    ]) :-
    post_type_ok(Port, Uuid),
    get_type(Port, Uuid, _{msg:"Found types", types:[_]}).

test(
    "Runs an example client session",
    [
        setup(server(Port, silent)),
        nondet,
        cleanup(shutdown(Port))
    ]) :-
    post_type_ok(Port, Uuid),
    get_type(Port, Uuid, _{msg:"Found types", types:[JsonType]}),
    % Delete all copies of Set.
    assertion(
        (jsonify_type(Ty, JsonType), Ty = type(Uuid, "Set", [generic("X", ["Add"])], ["Add"], ""))
    ),
    atom_string(Uuid, Uuids),
    delete_type(Port, Uuid, _{msg:"Removed type", uuid:Uuids}),
    % Check that we did in fact delete parseInt55 - this should be 404
    % If not, we don't throw an exception and read fail.
    catch((get_type(Port, Uuid, _), fail), _, true).

:- end_tests('type endpoint tests').
