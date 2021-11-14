:- use_module(library(http/http_client)).

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
        Reply,
        []
    ),
    ends_with_newline('Created func parseInt55', Reply). 

%% Runs a request which gets parseInt55 and unfies the response with Rpely
get(Reply) :-
    get_port(Port),
    http_get([
            protocol(http),
            host(localhost),
            port(Port),
            path('/func?name=parseInt[0-9][0-9]&name_cmp=re')
        ],
        Reply,
        []
    ).

%% Runs a request which deletes parseInt55 and unfies the response with Rpely
delete(Reply) :-
    get_port(Port),
    http_delete([
            protocol(http),
            host(localhost),
            port(Port),
            path('/func?name=parseInt[0-9][0-9]&name_cmp=re')
        ],
        Reply,
        []
    ).

subprocess(Write) :-
    get_port(Port),
    number_string(Port, PortS),
    process_create(
        path(swipl),
        ['main.pl', 'launch', PortS],
        [stdin(Write)]
    ).

:- begin_tests('end-to-end test').

test('Runs an example client session', [nondet]) :-
    % When server starts, parseInt55 does not exist, so it fails.
    delete(DeleteMissing),
    assertion(
        ends_with_newline(
            'No matches found',
            DeleteMissing
        )
    ),
    % parseInt55 does not exist and should not be found
    get(GetMissing),
    assertion(
        ends_with_newline(
            'No matching func found: parseInt[0-9][0-9] :: ? -> ? | none',
            GetMissing
        )
    ),
    % Adding items should be fine even if repeated.
    post_ok,
    post_ok,
    % parseInt55 should now exist.
    get(GetFound),
    assertion(
        ends_with_newline(
            'Found func: parseInt55 :: [str] -> [int] | Hello world!',
            GetFound
        )
    ),
    % Delete all copies of parseInt55.
    delete(DeleteFound),
    assertion(
        ends_with_newline(
            'Removed func parseInt55, parseInt55',
            DeleteFound
        )
    ),
    % Check that we did in fact delete parseInt55
    get(GetAfterDelete),
    assertion(
        ends_with_newline(
            'No matching func found: parseInt[0-9][0-9] :: ? -> ? | none',
            GetAfterDelete
        )
    ).

:- end_tests('end-to-end test').
