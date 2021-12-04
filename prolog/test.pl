% After you load this file in swipl, run with: run_tests.
% On the command line:
% * Use `make test-repl` to enter a repl with this file loaded.
% * Use `make test` to run the unit tests.
% * At the project run, use `make prolog-eval` to run the unit tests.
:- use_module(compat).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('sequence_ops').
:- use_module(sequence_ops).

test("split_left base case") :-
    split_left("Hello world !", " ", 0, ["Hello world !"]).
test("split_left split once") :-
    split_left("Hello   world !", " ", 1, ["Hello", "world !"]).
test("split_left split once multiple sep") :-
    split_left("Hello  : world :!", ": ", 1, ["Hello", "world :!"]).
test("split_left split many times") :-
    split_left("Hello    world  ! a a   a", " ", 999, ["Hello", "world", "!", "a", "a", "a"]).

test("levenshtein_distance all equal") :-
    levenshtein_distance("kitten", "kitten", 0.0).
test("levenshtein_distance insert and replace") :-
    levenshtein_distance("kitten", "knitting", 3.0).
test("levenshtein_distance no similarity") :-
    levenshtein_distance("abcdef", "zzzzzz", 6.0).
test("levenshtein_distance left empty") :-
    levenshtein_distance("", "zzzzzz", 6.0).
test("levenshtein_distance right empty") :-
    levenshtein_distance("abcdef", "", 6.0).

test("list_subset two empty lists") :-
    list_subset([], []).
test("list_subset empty sublist") :-
    list_subset([], [1,2,3]).
test("list_subset identical lists") :-
    list_subset([1,2,3], [1,2,3]).
test("list_subset has subset in order") :-
    list_subset([1,2,3], [1,2,3,4,5,6]).
test("list_subset has subset out of order") :-
    list_subset([1,3,2], [6,3,4,2,5,1]).
test("list_subset empty main list", [fail]) :-
    list_subset([1,2,3], []).
test("list_subset not a subset", [fail]) :-
    list_subset([1,2,3], [4,5,6]).

:- end_tests('sequence_ops').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('function').
:- use_module(function).

define_helper(Expr, Uuid) :-
    add_function(Uuid, "example-fn", _, ["type1"], ["type2"], "documentation"),
    Expr,
    retract(function(Uuid, _, _, _, _, _)).

test("name getter", [nondet]) :-
    define_helper(fname(Uuid, "example-fn"), Uuid).
test("doc getter", [nondet]) :-
    define_helper(docs(Uuid, "documentation"), Uuid).
test("input getter", [nondet]) :-
    define_helper(inputs(Uuid, ["type1"]), Uuid).
test("output getter", [nondet]) :-
    define_helper(outputs(Uuid, ["type2"]), Uuid).
test("output getter fails", [nondet]) :-
    define_helper(not(outputs("example-a", ["type2"])), _).

:- end_tests('function').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('function/parse').
:- use_module(function/parse).

test("parse signature succeeds docs") :-
    parse_signature("f :: [x] -> [y] | docs", "f", [], ["x"], ["y"], "docs").
test("parse signature succeeds empty docs") :-
    parse_signature("f :: [x] -> [y] | ", "f", [], ["x"], ["y"], "").
test("parse signature succeeds no docs") :-
    parse_signature("f :: [x] -> [y]", "f", [], ["x"], ["y"], "").
test("parse signature generics") :-
    parse_signature("f<X: T + Q> :: [x] -> [y]", "f", [generic("X", ["T", "Q"])], ["x"], ["y"], "").

test("parse signature generics is subst") :-
    parse_signature(
        "f<X: T + Q> :: [X] -> [X]", "f",
        [generic("X", ["T", "Q"])], [gen("X")], [gen("X")], "").

test("parse signature generics is subst for nested type") :-
    parse_signature(
        "f<X: T + Q> :: [List<X>] -> [X]", "f",
        [generic("X", ["T", "Q"])], [type("List", [gen("X")], _)], [gen("X")], "").
    
test("parse one impl") :-
    parse_type("type str: Add", type(_, "str", [], ["Add"], "")).
test("parse two impls") :-
    parse_type("type str: Add + Sub", type(_, "str", [], ["Add", "Sub"], "")).


test("parse trait bounds") :-
    parse_trait("trait X: X + Y", trait("X", ["X", "Y"])).
test("parse trait no bounds") :-
    parse_trait("X", trait("X", [])).

:- end_tests('function/parse').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('function/serde').
:- use_module(storage).
:- use_module(function/serde).

test("jsonify list of traits empty") :-
    serde:jsonify_traits([], []).

test("jsonify list of traits two items") :-
    serde:jsonify_traits(
        [trait("Add", []), trait("Sub", [])],
        [_{name:"Add", bounds:[]}, _{name:"Sub", bounds:[]}]
    ).

test("jsonify type roundtrip suceeds") :-
    find_all_types(Types),
    serde:jsonify_types(Types, JTypes),
    serde:jsonify_types(NewTypes, JTypes),
    assertion(Types=NewTypes).

test("jsonify trait roundtrip suceeds") :-
    find_all_traits(Traits),
    serde:jsonify_traits(Traits, JTraits),
    serde:jsonify_traits(NewTraits, JTraits),
    assertion(Traits=NewTraits).

test("jsonify function roundtrip suceeds") :-
    find_all_functions(Functions),
    serde:jsonify_funcs(Functions, JFunctions),
    serde:jsonify_funcs(NewFunctions, JFunctions),
    assertion(Functions=NewFunctions).

:- end_tests('function/serde').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('storage').
:- use_module(storage).
:- use_module(library(http/json)).

test("find_all_functions correct length") :-
    find_all_functions(Functions),
    length(Functions, 9).

test("find_all_types correct length") :-
    find_all_types(Types),
    length(Types, 4).

test("find_all_traits correct length") :-
    find_all_traits(Traits),
    length(Traits, 1).

test("storage roundtrip succeeds", [nondet]) :-
    with_output_to(string(String0), (
        current_output(Stream0),
        store_knowledge_base(Stream0)
    )),
    storage:clear_knowledge_base,
    open_string(String0, Stream1),
    load_knowledge_base(Stream1),
    % Should only have 4 types, 1 trait, and 8 functions
    assertion(aggregate_all(count, function:type(_, _, _, _, _), 4)),
    assertion(aggregate_all(count, function:trait(_, _), 1)),
    assertion(aggregate_all(count, function:function(_, _, _, _, _, _), 9)).

:- end_tests('storage').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('search').
:- use_module(search).
:- use_module(func_constraints).

test("dfs works") :-
    aggregate_all(
        count,
        func_path_no_cycles(dfs, ["int"], ["int"], _Path),
        64
    ).

test("bfs works") :-
    aggregate_all(
        count,
        func_path_no_cycles(bfs, ["int"], ["int"], _Path),
        64
    ).
test("bestfs works") :-
    aggregate_all(
        count,
        func_path_no_cycles(bestfs, ["int"], ["int"], _Path),
        64
    ).

test("No paths for types which do not exist", [fail]) :-
    func_path_no_cycles(
        dfs,
        ["not a real type"],
        ["also not a real type"],
    _).

test("Regex matches all", [fail]) :-
    find_items(
        regex_constraint(".*", func_field(name)),
        Funcs
    ),
    length(Funcs, 8).

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
test("and_constraint no constraints succeeds with score of 0.0") :-
    function:fname(Uuid, "parseInt"),
    and_constraint([], Uuid, 0.0, and_constraint([])).
test("substring constraint succeeds", [nondet]) :-
    function:fname(Uuid, "parseInt"),
    substring_constraint("parse", func_field(name), Uuid, 0.0, no_constraint).
test("substring constraint fails") :-
    function:fname(Uuid, "parseInt"),
    substring_constraint("dsdsfdwa", func_field(name), Uuid, 1.0, _).
test("subsequence constraint", [nondet]) :-
    function:fname(Uuid, "parseInt"),
    subsequence_constraint("pre", func_field(name), Uuid, 0.0, no_constraint).
test("subsequence constraint fail") :-
    function:fname(Uuid, "parseInt"),
    subsequence_constraint("tspkn", func_field(name), Uuid, 1.0, _).
test("input constraint", [nondet]) :-
    function:fname(Uuid, "increment"),
    input_constraint(["int"], Uuid, 0.0, input_constraint(["int"])).
test("input constraint fails when input does not match", [fail]) :-
    function:fname(Uuid, "increment"),
    input_constraint(["str"], Uuid, 1.0, _).
test(
    "regex constraint",
    [condition(prolog_version_eight)]
    ) :-
    function:fname(Uuid, "decrement"),
    regex_constraint("de.*", func_field(name), Uuid, 0.0, no_constraint).
test(
    "regex constraint fail",
    [condition(prolog_version_eight)]
    ) :-
    function:fname(Uuid, "decrement"),
    regex_constraint("d.*A", func_field(name), Uuid, 1.0, _).

test("similarity constraint", [nondet]) :-
    function:fname(Uuid, "listify"),
    similarity_constraint("produces a list", docs, Uuid, _, no_constraint),
    similarity_constraint("not similar at all", docs, Uuid, 1.0, _).

test("sub_similarity constraint", [nondet]) :-
    function:fname(Uuid, "listify"),
    sub_similarity_constraint("produces a list", docs, Uuid, _, no_constraint),
    sub_similarity_constraint("not similar at all", docs, Uuid, 1.0, _).

test("sub_similarity constraint", [nondet]) :-
    function:fname(Uuid, "print"),
    sub_similarity_constraint("see also", docs, Uuid, _, no_constraint),
    sub_similarity_constraint("also see", docs, Uuid, _, no_constraint).

test(
    "at_most_n fails when it hits 0",
    [fail]
    ) :-
        at_most_n_constraint(0, no_constraint, _, _, _).
test("at_most_n succeeds if non-zero") :-
    at_most_n_constraint(1, no_constraint, _, _, at_most_n_constraint(0, no_constraint)).
test("at_most_n does not decrement if failure") :-
    at_most_n_constraint(1, [_, _, _]>>(fail), _, _, at_most_n_constraint(1, _)).

test("scale constraint") :-
    function:fname(Uuid, "parseInt"),
    scale_constraint(substring_constraint("dsdsfdwa", func_field(name)), 5.0, Uuid, 5.0, _).

test("scale constraint") :-
    function:fname(Uuid, "parseInt"),
    scale_constraint(substring_constraint("dsdsfdwa", func_field(name)), 0.3, Uuid, 0.3, _).

:- end_tests('func_constraints').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('path_constraints').
:- use_module(path_constraints).
test("cycle_constraint does not allow cycles", [fail]) :-
    cycle_constraint([a, b, c, d], d).
test("cycle_constraint allows none") :-
    cycle_constraint([a, b, c, d], none).
test("cycle_constraint allows non-cycle") :-
    cycle_constraint([a, b, c, d], e).

test("length_constraint fails on overly long path", [fail]) :-
    length_constraint(3, [a, b, c], d).
test("length_constraint is fine when path is short") :-
    length_constraint(4, [a, b, c], d).

test("length_constraint does not allow excessively long path at end", [fail]) :-
    length_constraint(3, [a, b, c, d], none).
test("length_constraint is fine testing end") :-
    length_constraint(3, [a, b, c], none).

:- end_tests('path_constraints').

