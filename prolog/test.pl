% Make a group of tests with begin_tests/end_tests.
% Make a test with test/2.
% Run your tests with run_tests/0.

% After you load this file in swipl, run with: run_tests.
% On the command line:
% * Use `make test-repl` to enter a repl with this file loaded.
% * Use `make test` to run the unit tests.
% * At the project run, use `make prolog-eval` to run the unit tests.
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

:- begin_tests('function').
:- use_module(function).

define_helper(Expr) :-
    assertz(function("example-fn", ["type1"], ["type2"], "documentation")),
    Expr,
    retract(function("example-fn", ["type1"], ["type2"], "documentation")).

test('name getter', [nondet]) :-
    define_helper(name("example-fn", "example-fn")).
test('doc getter', [nondet]) :-
    define_helper(docs("example-fn", "documentation")).
test('input getter', [nondet]) :-
    define_helper(inputs("example-fn", ["type1"])).
test('output getter', [nondet]) :-
    define_helper(outputs("example-fn", ["type2"])).
test('output getter fails', [nondet]) :-
    define_helper(not(outputs("example-a", ["type2"]))).

:- end_tests('function').

:- begin_tests('search').
:- use_module(search).

:- end_tests('search').

:- begin_tests('func_constraints').
:- use_module(func_constraints).

:- end_tests('func_constraints').

:- begin_tests('path_constraints').
:- use_module(path_constraints).

:- end_tests('path_constraints').

