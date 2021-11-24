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
    http_post([
            protocol(http),
            host(localhost),
            port(Port),
            path('/func')
        ],
        form_data([
            name="parseInt55",
            inputs="str",
            outputs="int",
            docs="Hello world!"
        ]),
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

:- begin_tests('end-to-end function test', [condition(prolog_version_eight)]).

test(
    "Trying to delete nonexistent uuid results in 404",
    [
        setup(server(Port)),
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
        setup(server(Port)),
        error(existence_error(_, _)),
        cleanup(shutdown(Port))
    ]) :-
    get_fn(Port, _{
        msg:"No matching func found: pant5 :: ? -> ? | none"
    }).

test(
    "Runs an example client session",
    [
        setup(server(Port)),
        nondet,
        cleanup(shutdown(Port))
    ]) :-
    % parseInt55 does not exist and should not be found
    % Adding items should be fine even if repeated.
    % parseInt55 should now exist.
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
        delete_fn(Port, Uuid1, _{msg: "Removed", uuid:Uuid1})
    ),
    assertion(
    delete_fn(Port, Uuid2, _{msg: "Removed", uuid:Uuid2})
    ),
    % Check that we did in fact delete parseInt55 - this should be 404
    % If not, we don't throw an exception and read fail.
    catch((get_fn(Port, _), fail), _, true).

:- end_tests('end-to-end function test').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Defining, searching, and deleting types

%% Checks that posting AddSet<X: Add> works correctly
post_type_ok(Port) :-
    jsonify_type(type("Set", [generic("X", ["Add"])], ["Add"]), JsonType),
    atom_json_dict(Type, JsonType, [as(atom)]),
    http_post([
            protocol(http),
            host(localhost),
            port(Port),
            path('/type')
        ],
        atom('application/json', Type),
        _{
            msg:"Created type Set"
        },
        [json_object(dict)]
    ).

%% Runs a request which gets parseInt55 and unfies the response with Rpely
get_type(Port, Reply) :-
    http_get([
            protocol(http),
            host(localhost),
            port(Port),
            path('/type?name=Set')
        ],
        Reply,
        [json_object(dict)]
    ).

%% Runs a request which deletes parseInt55 and unfies the response with Rpely
delete_type(Port, Name, Reply) :-
    atom_concat('/type?name=', Name, Path),
    http_delete([
            protocol(http),
            host(localhost),
            port(Port),
            path(Path)
        ],
        Reply,
        [json_object(dict)]
    ).
:- begin_tests('end-to-end type test', [condition(prolog_version_eight)]).

test(
    "Trying to delete nonexistent type results in 404",
    [
        setup(server(Port)),
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
        setup(server(Port)),
        error(existence_error(_, _)),
        cleanup(shutdown(Port))
    ]) :-
    get_type(Port, _{
        msg:"No type with name Set found."
    }).

test(
    "Post suceeds",
    [
        setup(server(Port)),
        nondet,
        cleanup(shutdown(Port))
    ]) :-
    post_type_ok(Port).

test(
    "Runs an example client session",
    [
        setup(server(Port)),
        nondet,
        cleanup(shutdown(Port))
    ]) :-
    post_type_ok(Port),
    post_type_ok(Port),
    get_type(Port, _{msg:"Found type", type:JsonType}),
    % Delete all copies of Set.
    assertion(
        jsonify_type(type("Set", [generic("X", ["Add"])], ["Add"]), JsonType)
    ),
    delete_type(Port, 'Set', _{msg: "Removed type", name:"Set"}),
    % Check that we did in fact delete parseInt55 - this should be 404
    % If not, we don't throw an exception and read fail.
    catch((get_type(Port, _), fail), _, true).

:- end_tests('end-to-end type test').
