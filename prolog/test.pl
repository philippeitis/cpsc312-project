% After you load this file in swipl, run with: run_tests.
% On the command line:
% * Use `make test-repl` to enter a repl with this file loaded.
% * Use `make test` to run the unit tests.
% * At the project run, use `make prolog-eval` to run the unit tests.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('string_ops').
:- use_module(string_op).

test('split_left base case', [nondet]) :-
    split_left("Hello world !", " ", 0, ["Hello world !"]).
test('split_left split once', [nondet]) :-
    split_left("Hello   world !", " ", 1, ["Hello", "world !"]).
test('split_left split once multiple sep', [nondet]) :-
    split_left("Hello  : world :!", ": ", 1, ["Hello", "world :!"]).
test('split_left split many times', [nondet]) :-
    split_left("Hello    world  ! a a   a", " ", 999, ["Hello", "world", "!", "a", "a", "a"]).

test('levenshtein_distance all equal', [nondet]) :-
    levenshtein_distance("kitten", "kitten", 0).
test('levenshtein_distance insert and replace', [nondet]) :-
    levenshtein_distance("kitten", "knitting", 3).
test('levenshtein_distance no similarity', [nondet]) :-
    levenshtein_distance("abcdef", "zzzzzz", 6).
test('levenshtein_distance left empty', [nondet]) :-
    levenshtein_distance("", "zzzzzz", 6).
test('levenshtein_distance right empty', [nondet]) :-
    levenshtein_distance("abcdef", "", 6).

:- end_tests('string_ops').

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

test('parse signature succeeds docs', [nondet]) :-
    parse_signature("f :: [x] -> [y] | docs", "f", [], ["x"], ["y"], "docs").
test('parse signature succeeds empty docs', [nondet]) :-
    parse_signature("f :: [x] -> [y] | ", "f", [], ["x"], ["y"], "").
test('parse signature succeeds no docs', [nondet]) :-
    parse_signature("f :: [x] -> [y]", "f", [], ["x"], ["y"], "").
test('parse signature generics', [nondet]) :-
    parse_signature("f<X: T + Q> :: [x] -> [y]", "f", [generic("X", ["T", "Q"])], ["x"], ["y"], "").

test('parse one impl', [nondet]) :-
    parse_impls("str impls Add", "str", ["Add"]).
test('parse two impls', [nondet]) :-
    parse_impls("str impls Add + Sub", "str", ["Add", "Sub"]).


test('parse trait bounds', [nondet]) :-
    parse_trait("X: X + Y", trait("X", ["X", "Y"])).
test('parse trait no bounds', [nondet]) :-
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
    open_string(String0, Stream1),
    function:clear_funcs,
    read_json_metadata(Stream1),
    with_output_to(string(String1), (
        current_output(Stream2),
        write_json_metadata(Stream2)
    )),
    open_string(String0, Stream3),
    open_string(String1, Stream4),
    json_read_dict(Stream3, First),
    json_read_dict(Stream4, Second),
    assertion(First = Second).

:- end_tests('function/serde').

:- begin_tests('search').
:- use_module(search).

test('specialization of add is not skipped', [nondet]) :-
    findall(
        Path,
        func_path_no_cycles(["int"], ["int"], Path),
        Paths
    ),
    length(Paths, 15).
test('No paths for types which do not exist', [nondet]) :-
    \+func_path_no_cycles(
        ["not a real type"],
        ["also not a real type"],
    _).

:- end_tests('search').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('func_constraints').
:- use_module(func_constraints).

:- end_tests('func_constraints').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('path_constraints').
:- use_module(path_constraints).
test('cycle_constraint does not allow cycles', [nondet]) :-
    \+cycle_constraint([a, b, c, d], d, _).
test('cycle_constraint allows none', [nondet]) :-
    cycle_constraint([a, b, c, d], none, _).
test('cycle_constraint allows non-cycle', [nondet]) :-
    cycle_constraint([a, b, c, d], e, _).

test('length_constraint fails on overly long path', [nondet]) :-
    \+length_constraint([a, b, c], d, 3).
test('length_constraint is fine when path is short', [nondet]) :-
    length_constraint([a, b, c], d, 4).

test('length_constraint does not allow excessively long path at end', [nondet]) :-
    \+length_constraint([a, b, c, d], none, 3).
test('length_constraint is fine testing end', [nondet]) :-
    length_constraint([a, b, c], none, 3).

:- end_tests('path_constraints').

