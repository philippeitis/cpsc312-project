:- use_module(library(http/http_client)).
:- use_module(server).

%% Checks that posting parseInt55 works correctly
post_ok(Port) :-
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
get(Port, Reply) :-
    http_get([
            protocol(http),
            host(localhost),
            port(Port),
            path('/func?name=parseInt[0-9][0-9]&name_cmp=re')
        ],
        Reply,
        [json_object(dict)]
    ).

%% Runs a request which deletes parseInt55 and unfies the response with Rpely
delete(Port, Uuid, Reply) :-
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

:- begin_tests('end-to-end test').

test(
    "Trying to delete nonexistent uuid results in 404",
    [
        setup(server(Port)),
        error(existence_error(_, _)),
        cleanup(shutdown(Port))
    ]) :-
    % Should give status 404
    delete(Port, '0', _{
        msg:"No matches found for provided query."
    }).

test(
    "Trying to get nonexistent function results in 404",
    [
        setup(server(Port)),
        error(existence_error(_, _)),
        cleanup(shutdown(Port))
    ]) :-
    get(Port, _{
        msg:"No matching func found: parseInt[0-9][0-9] :: ? -> ? | none"
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
    post_ok(Port),
    post_ok(Port),
    get(Port, _{msg:"Found functions", functions:[
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
        delete(Port, Uuid1, _{msg: "Removed", uuid:Uuid1})
    ),
    assertion(
        delete(Port, Uuid2, _{msg: "Removed", uuid:Uuid2})
    ),
    % Check that we did in fact delete parseInt55 - this should be 404
    % If not, we don't throw an exception and read fail.
    catch((get(Port, _), fail), _, true).

:- end_tests('end-to-end test').
