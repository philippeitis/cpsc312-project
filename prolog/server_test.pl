:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

newline("\r\n").
newline("\n").

get_port(Port) :-
    getenv("FASTFUNC_SERVER_PORT", PortS),
    atom_number(PortS, Port), !.
get_port(5000).

ends_with_newline(RootA, StringA) :-
    newline(NL),
    atom_string(RootA, Root),
    atom_string(StringA, String),
    string_concat(Root, NL, String), !.

%% Checks that posting parseInt55 works correctly
post_ok :-
    get_port(Port),
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
        ReplyString,
        []
    ),
    open_string(ReplyString, ReplyStream),
    json_read_dict(ReplyStream, _{
        msg:"Created func parseInt55",
        uuid:_
    }). 

%% Runs a request which gets parseInt55 and unfies the response with Rpely
get(Reply) :-
    get_port(Port),
    http_get([
            protocol(http),
            host(localhost),
            port(Port),
            path('/func?name=parseInt[0-9][0-9]&name_cmp=re')
        ],
        ReplyString,
        []
    ),
    open_string(ReplyString, ReplyStream),
    json_read_dict(ReplyStream, Reply).

%% Runs a request which deletes parseInt55 and unfies the response with Rpely
delete(Uuid, Reply) :-
    get_port(Port),
    atom_concat('/func?uuid=', Uuid, Path),
    http_delete([
            protocol(http),
            host(localhost),
            port(Port),
            path(Path)
        ],
        ReplyString,
        []
    ), 
    open_string(ReplyString, ReplyStream),
    json_read_dict(ReplyStream, Reply).

subprocess(Write) :-
    get_port(Port),
    number_string(Port, PortS),
    process_create(
        path(swipl),
        ['main.pl', 'launch', PortS],
        [stdin(Write)]
    ).

:- begin_tests('end-to-end test').

test('Tries to delete nonexistent uuid results in 404', [error(existence_error(_, _))]) :-
    % Should give status 404
    delete('0', _{
        msg:"No matches found for provided query."
    }).

test('Tries to get nonexistent function results in 404', [error(existence_error(_, _))]) :-
    get(_{
        msg:"No matching func found: parseInt[0-9][0-9] :: ? -> ? | none"
    }).

test('Runs an example client session', [nondet]) :-
    % parseInt55 does not exist and should not be found
    % Adding items should be fine even if repeated.
    post_ok,
    post_ok,
    % parseInt55 should now exist.
    get(_{msg:"Found functions", functions:[
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
        delete(Uuid1, _{msg: "Removed", uuid:Uuid1})
    ),
    assertion(
        delete(Uuid2, _{msg: "Removed", uuid:Uuid2})
    ),
    % Check that we did in fact delete parseInt55 - this should be 404
    catch(get(_), _, true).

:- end_tests('end-to-end test').
