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

test("split_left split string start") :-
    split_left("      Hello   world !", " ", 1, ["Hello", "world !"]).

test("split_left split once") :-
    split_left("Hello   world !", " ", 1, ["Hello", "world !"]).
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

example_fn(function('0123', "example-fn", [], ["type1"], ["type2"], "documentation")).

test("name getter") :-
    example_fn(Fn),
    fname(Fn, "example-fn").
test("doc getter") :-
    example_fn(Fn),
    docs(Fn, "documentation").
test("input getter") :-
    example_fn(Fn),
    inputs(Fn, ["type1"]).
test("output getter") :-
    example_fn(Fn),
    outputs(Fn, ["type2"]).

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
    serde:jsonify_fns(Functions, JFunctions),
    serde:jsonify_fns(NewFunctions, JFunctions),
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
:- use_module(string_constraints).
:- use_module(func_constraints).
:- use_module(constraints).

test("dfs works") :-
    findall(
        Path,
        func_path_no_cycles(dfs, ["int"], ["int"], Path),
        Paths
    ),
    length(Paths, 64).

test("bfs works") :-
    findall(
        Path,
        func_path_no_cycles(bfs, ["int"], ["int"], Path),
        Paths
    ),
    length(Paths, 64).

test("bestfs works") :-
    findall(
        Path,
        func_path_no_cycles(dfs, ["int"], ["int"], Path),
        Paths
    ),
    length(Paths, 64).

test("No paths for types which do not exist", [fail]) :-
    func_path_no_cycles(
        dfs,
        ["not a real type"],
        ["also not a real type"],
    _).

test("Regex matches all") :-
    func_constraints:fn_member_constraint(Constraint),
    find_items(
        constraints:and_constraint(
            Constraint,
            string_constraints:regex_constraint(function:func_field(name), ".*")
        ),
        Funcs
    ),
    length(Funcs, 14).

test("Regex with empty and matches all") :-
    func_constraints:fn_member_constraint(Constraint),
    find_items(
        constraints:and_constraint(
            Constraint,
            constraints:and_constraint(
                string_constraints:regex_constraint(function:func_field(name), ".*"),
                no_constraint
            )
        ),
        Funcs
    ),
    length(Funcs, 14).

test("Regex with empty input constraint matches all") :-
    func_constraints:fn_member_constraint(Constraint),
    find_items(
        constraints:and_constraint(
            Constraint,
            constraints:and_constraint(
                string_constraints:regex_constraint(function:func_field(name), ".*"),
                func_constraints:input_constraint([])
            )
        ),
        Funcs
    ),
    length(Funcs, 14).

test("Regex with empty input constraint matches all except generic") :-
    func_constraints:fn_member_constraint(Constraint),
    find_items(
        constraints:and_constraint(
            Constraint,
            constraints:and_constraint(
                string_constraints:regex_constraint(function:func_field(name), ".*"),
                func_constraints:input_constraint([])
            )
        ),
        Funcs
    ),
    length(Funcs, 14).

test("And constraint order irrelevant") :-
    func_constraints:fn_member_constraint(Constraint),
    find_items(
        constraints:and_constraint(
            Constraint,
            constraints:and_constraint(
                func_constraints:input_constraint([]),
                string_constraints:regex_constraint(function:func_field(name), ".*")
            )
        ),
        Funcs
    ),
    length(Funcs, 14).

test("And constraint nested") :-
    func_constraints:fn_member_constraint(Constraint),
    find_items(
        constraints:and_constraint(
            Constraint,
            constraints:and_constraint(
                func_constraints:input_constraint([]),
                and_constraint(
                    func_constraints:output_constraint([]),
                    and_constraint(
                        string_constraints:regex_constraint(function:func_field(name), ".*"),
                        no_constraint
                    )
                )
            )
        ),
        Funcs
    ),
    length(Funcs, 14).

:- end_tests('search').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('func_constraints').
:- use_module(func_constraints).

test("input constraint", [nondet]) :-
    function:fname(Uuid, "increment"),
    input_constraint(["int"], Uuid, 0.0, input_constraint(["int"])).
test("input constraint fails when input does not match", [fail]) :-
    function:fname(Uuid, "increment"),
    input_constraint(["str"], Uuid, 1.0, _).

:- end_tests('func_constraints').

:- begin_tests('string_constraints').
:- use_module(function, [func_field/3]).
:- use_module(string_constraints).

%% Constraint detail: Some constraints have hard failures, while some have
% soft failures, allowing a path to continue. In this case,
% string constraints simply repeat themselves, as they can be satisfied
% at any point, and we simply check that there are no constraints left
% when the path is complete. However, input constraints will fail if
% the input does not match, as this would allow an invalid path.
test("substring constraint succeeds") :-
    function:get_function(_, Func),
    function:fname(Func, "parseInt"), !,
    substring_constraint(func_field(name), "parse", Func, 0.0, no_constraint).
test("substring constraint fails") :-
    function:get_function(_, Func),
    function:fname(Func, "parseInt"), !,
    substring_constraint(func_field(name), "dsdsfdwa", Func, 1.0, _).
test("subsequence constraint") :-
    function:get_function(_, Func),
    function:fname(Func, "parseInt"), !,
    subsequence_constraint(func_field(name), "pre", Func, 0.0, no_constraint).
test("subsequence constraint fail") :-
    function:get_function(_, Func),
    function:fname(Func, "parseInt"), !,
    subsequence_constraint(func_field(name), "tspkn", Func, 1.0, _).
test(
    "regex constraint",
    [condition(prolog_version_eight)]
    ) :-
    function:get_function(_, Func),
    function:fname(Func, "decrement"), !,
    regex_constraint(func_field(name), "de.*", Func, 0.0, no_constraint).
test(
    "regex constraint fail",
    [condition(prolog_version_eight)]
    ) :-
    function:get_function(_, Func),
    function:fname(Func, "decrement"), !,
    regex_constraint(func_field(name), "d.*A", Func, 1.0, _).

test("similarity constraint", [nondet]) :-
    function:get_function(_, Func),
    function:fname(Func, "listify"), !,
    similarity_constraint(func_field(docs), "produces a list", Func, _, no_constraint),
    similarity_constraint(func_field(docs), "not similar at all", Func, 1.0, _).

test("sub_similarity constraint", [nondet]) :-
    function:get_function(_, Func),
    function:fname(Func, "listify"), !,
    sub_similarity_constraint(func_field(docs), "produces a list", Func, _, no_constraint),
    sub_similarity_constraint(func_field(docs), "not similar at all", Func, 1.0, _).

test("sub_similarity constraint ignores order") :-
    function:get_function(_, Func),
    function:fname(Func, "print"), !,
    sub_similarity_constraint(func_field(docs), "see also", Func, _, no_constraint),
    sub_similarity_constraint(func_field(docs), "also see", Func, _, no_constraint).

:- end_tests('string_constraints').

:- begin_tests('constraints').
:- use_module(constraints).

identity(X, X).

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
    scale_constraint(string_constraints:substring_constraint(function:func_field(name), "dsdsfdwa"), 5.0, Uuid, 5.0, _).

test("scale constraint") :-
    function:fname(Uuid, "parseInt"),
    scale_constraint(string_constraints:substring_constraint(function:func_field(name), "dsdsfdwa"), 0.3, Uuid, 0.3, _).

test("cycle_constraint does not allow cycles", [fail]) :-
    cycle_constraint(identity, [a, b, c, d], d, _, _).
test("cycle_constraint allows non-cycle") :-
    cycle_constraint(identity, [a, b, c, d], e, _, _).

:- end_tests('constraints').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- begin_tests('path_constraints').
:- use_module(path_constraints).

:- end_tests('path_constraints').

