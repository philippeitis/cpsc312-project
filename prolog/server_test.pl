:- use_module(library(http/http_client)).

%% Checks that posting parseInt55 works correctly
post_ok :-
    http_post([
            protocol(http),
            host(localhost),
            port(5000),
            path('/func')
        ],
        form_data([
            name="parseInt55",
            inputs="str",
            outputs="int",
            docs="Hello world!"
        ]),
        'Created func parseInt55\n',
        []
    ).

%% Runs a request which gets parseInt55 and unfies the response with Rpely
get(Reply) :-
    http_get([
            protocol(http),
            host(localhost),
            port(5000),
            path('/func?name=parseInt[0-9][0-9]&name_cmp=re')
        ],
        Reply,
        []
    ).

%% Runs a request which deletes parseInt55 and unfies the response with Rpely
delete(Reply) :-
    http_delete([
            protocol(http),
            host(localhost),
            port(5000),
            path('/func?name=parseInt[0-9][0-9]&name_cmp=re')
        ],
        Reply,
        []
    ).

subprocess(Write) :-
    process_create(
        path(swipl),
        ['main.pl', 'launch', '5000'],
        [stdin(Write)]
    ).

:- begin_tests('end-to-end test').

test('Runs an example client session', [nondet]) :-
    % When server starts, parseInt55 does not exist, so it fails.
    delete(DeleteMissing),
    assertion(DeleteMissing == 'Deletion failed\n'),
    % parseInt55 does not exist and should not be found
    get(GetMissing),
    assertion(GetMissing == 'No matching func found: parseInt[0-9][0-9] :: ? -> ? | none\n'),
    % Adding items should be fine even if repeated.
    post_ok,
    post_ok,
    % parseInt55 should now exist.
    get(GetFound),
    assertion(GetFound == 'Found func: parseInt55 :: [str] -> [int] | Hello world!\n'),
    % Delete all copies of parseInt55.
    delete(DeleteFound),
    assertion(DeleteFound == 'Removed func parseInt55\n'),
    % Check that we did in fact delete parseInt55
    get(GetAfterDelete),
    assertion(GetAfterDelete == 'No matching func found: parseInt[0-9][0-9] :: ? -> ? | none\n').

:- end_tests('end-to-end test').
