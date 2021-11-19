% After you load this file in swipl, run with: run_tests.
% On the command line:
% * Use `make test-repl` to enter a repl with this file loaded.
% * Use `make test` to run the unit tests.
% * At the project run, use `make prolog-eval` to run the unit tests.
:- use_module(compat).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('sequence_ops').
:- use_module(sequence_ops).

test('split_left base case') :-
    split_left("Hello world !", " ", 0, ["Hello world !"]).
test('split_left split once') :-
    split_left("Hello   world !", " ", 1, ["Hello", "world !"]).
test('split_left split once multiple sep') :-
    split_left("Hello  : world :!", ": ", 1, ["Hello", "world :!"]).
test('split_left split many times') :-
    split_left("Hello    world  ! a a   a", " ", 999, ["Hello", "world", "!", "a", "a", "a"]).

test('levenshtein_distance all equal') :-
    levenshtein_distance("kitten", "kitten", 0).
test('levenshtein_distance insert and replace') :-
    levenshtein_distance("kitten", "knitting", 3).
test('levenshtein_distance no similarity') :-
    levenshtein_distance("abcdef", "zzzzzz", 6).
test('levenshtein_distance left empty') :-
    levenshtein_distance("", "zzzzzz", 6).
test('levenshtein_distance right empty') :-
    levenshtein_distance("abcdef", "", 6).

test('list_subset two empty lists') :-
    list_subset([], []).
test('list_subset empty sublist') :-
    list_subset([], [1,2,3]).
test('list_subset identical lists') :-
    list_subset([1,2,3], [1,2,3]).
test('list_subset has subset in order') :-
    list_subset([1,2,3], [1,2,3,4,5,6]).
test('list_subset has subset out of order') :-
    list_subset([1,3,2], [6,3,4,2,5,1]).
test('list_subset empty main list', [fail]) :-
    list_subset([1,2,3], []).
test('list_subset not a subset', [fail]) :-
    list_subset([1,2,3], [4,5,6]).

:- end_tests('sequence_ops').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('function').
:- use_module(function).

define_helper(Expr, Uuid) :-
    add_function(Uuid, "example-fn", _, ["type1"], ["type2"], "documentation"),
    Expr,
    retract(function(Uuid, _, _, _, _, _)).

test('name getter', [nondet]) :-
    define_helper(fname(Uuid, "example-fn"), Uuid).
test('doc getter', [nondet]) :-
    define_helper(docs(Uuid, "documentation"), Uuid).
test('input getter', [nondet]) :-
    define_helper(inputs(Uuid, ["type1"]), Uuid).
test('output getter', [nondet]) :-
    define_helper(outputs(Uuid, ["type2"]), Uuid).
test('output getter fails', [nondet]) :-
    define_helper(not(outputs("example-a", ["type2"])), _).

:- end_tests('function').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('function/parse').
:- use_module(function/parse).

test('parse signature succeeds docs') :-
    parse_signature("f :: [x] -> [y] | docs", "f", [], ["x"], ["y"], "docs").
test('parse signature succeeds empty docs') :-
    parse_signature("f :: [x] -> [y] | ", "f", [], ["x"], ["y"], "").
test('parse signature succeeds no docs') :-
    parse_signature("f :: [x] -> [y]", "f", [], ["x"], ["y"], "").
test('parse signature generics') :-
    parse_signature("f<X: T + Q> :: [x] -> [y]", "f", [generic("X", ["T", "Q"])], ["x"], ["y"], "").

test('parse signature generics is subst') :-
    parse_signature(
        "f<X: T + Q> :: [X] -> [X]", "f",
        [generic("X", ["T", "Q"])], [gen("X")], [gen("X")], "").

test('parse one impl') :-
    parse_type("type str: Add", type("str", [], ["Add"])).
test('parse two impls') :-
    parse_type("type str: Add + Sub", type("str", [], ["Add", "Sub"])).


test('parse trait bounds') :-
    parse_trait("trait X: X + Y", trait("X", ["X", "Y"])).
test('parse trait no bounds') :-
    parse_trait("X", trait("X", [])).

:- end_tests('function/parse').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('function/serde').
:- use_module(function/serde).
:- use_module(library(http/json)).

test('json roundtrip succeeds', [nondet]) :-
    with_output_to(string(String0), (
        current_output(Stream0),
        write_json_metadata(Stream0)
    )),
    function:clear_kb,
    open_string(String0, Stream1),
    read_json_metadata(Stream1),
    with_output_to(string(String1), (
        current_output(Stream2),
        write_json_metadata(Stream2)
    )),
    open_string(String0, Stream3),
    open_string(String1, Stream4),
    json_read_dict(Stream3, JsonMetadata),
    json_read_dict(Stream4, JsonMetadata),
    % Should only have 4 types, 1 trait, and 8 functions
    assertion(aggregate_all(count, function:type(_, _, _), 4)),
    assertion(aggregate_all(count, function:trait(_, _), 1)),
    assertion(aggregate_all(count, function:function(_, _, _, _, _, _), 9)).

test('jsonify list of traits empty') :-
    serde:jsonify_list_of_traits([], []).

test('jsonify list of traits two items') :-
    serde:jsonify_list_of_traits(
        [trait("Add", []), trait("Sub", [])],
        [_{name:"Add", bounds:[]}, _{name:"Sub", bounds:[]}]
    ).

:- end_tests('function/serde').

:- begin_tests('search').
:- use_module(search).

test('specialization of add is not skipped') :-
    aggregate_all(
        count,
        func_path_no_cycles(dfs, ["int"], ["int"], _Path),
        64
    ).
test('bfs works') :-
    aggregate_all(
        count,
        func_path_no_cycles(bfs, ["int"], ["int"], _Path),
        64
    ).
test('bestfs works') :-
    aggregate_all(
        count,
        func_path_no_cycles(bestfs, ["int"], ["int"], _Path),
        64
    ).

test('No paths for types which do not exist', [fail]) :-
    func_path_no_cycles(
        dfs,
        ["not a real type"],
        ["also not a real type"],
    _).

:- end_tests('search').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('func_constraints').
:- use_module(func_constraints).

%% Constraint detail: Some constraints have hard failures, while some have
% soft failures, allowing a path to continue. In this case,
% string constraints simply repeat themselves, as they can be satisfied
% at any point, and we simply check that there are no constraints left
% when the path is complete. However, input constraints will fail if
% the input does not match, as this would allow an invalid path.
test('func_constraints no constraints succeeds with score of 0.0') :-
    function:fname(Uuid, "parseInt"),
    func_constraints(Uuid, [], 0.0, []).
test('func constraints substring constraint', [nondet]) :-
    function:fname(Uuid, "parseInt"),
    func_constraints(Uuid, [(substring_constraint, ("parse", name))], 0.0, _).
test('func constraints substring constraint fail') :-
    function:fname(Uuid, "parseInt"),
    func_constraints(Uuid, [(substring_constraint, ("dsdsfdwa", name))], 1.0, _).
test('func constraints subsequence constraint', [nondet]) :-
    function:fname(Uuid, "parseInt"),
    func_constraints(Uuid, [(subsequence_constraint, ("pre", name))], 0.0, _).
test('func constraints subsequence constraint fail') :-
    function:fname(Uuid, "parseInt"),
    func_constraints(Uuid, [(subsequence_constraint, ("tspkn", name))], 1.0, _).
test('func constraints input constraint', [nondet]) :-
    function:fname(Uuid, "increment"),
    input_constraint(Uuid, ["int"], 0.0, (input_constraint, ["int"])).
test('func constraints input constraint fails when input does not match', [fail]) :-
    function:fname(Uuid, "increment"),
    input_constraint(Uuid, ["str"], 1.0, _).
test(
    'func constraints regex constraint',
    [
        nondet,
        condition(can_use_regex)
    ]
    ) :-
    function:fname(Uuid, "decrement"),
    func_constraints(Uuid, [(regex_constraint, ("decrement", name))], 0.0, _).
test(
    'func constraints regex constraint',
    [condition(can_use_regex)]
    ) :-
    function:fname(Uuid, "decrement"),
    regex_constraint(Uuid, ("de.*", name), 0.0, (no_constraint, _)).
test(
    'func constraints regex constraint fail',
    [condition(can_use_regex)]
    ) :-
    function:fname(Uuid, "decrement"),
    regex_constraint(Uuid, ("d.*A", name), 1.0, _).

:- end_tests('func_constraints').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('path_constraints').
:- use_module(path_constraints).
test('cycle_constraint does not allow cycles', [fail]) :-
    cycle_constraint([a, b, c, d], d, _).
test('cycle_constraint allows none') :-
    cycle_constraint([a, b, c, d], none, _).
test('cycle_constraint allows non-cycle') :-
    cycle_constraint([a, b, c, d], e, _).

test('length_constraint fails on overly long path', [fail]) :-
    length_constraint([a, b, c], d, 3).
test('length_constraint is fine when path is short') :-
    length_constraint([a, b, c], d, 4).

test('length_constraint does not allow excessively long path at end', [fail]) :-
    length_constraint([a, b, c, d], none, 3).
test('length_constraint is fine testing end') :-
    length_constraint([a, b, c], none, 3).

:- end_tests('path_constraints').