:- module(search, [
    func_path/4,
    func_path_no_cycles/4,
    find_item/3,
    find_items/2,
    func_search/7
]).
:- use_module(function).
:- use_module(constraints, [and_constraint/5, at_most_n_constraint/5, no_constraint/3]).
:- use_module(func_constraints, [input_constraint/4, output_constraint/4]).
:- use_module(string_constraints, [add_string_constraint/5]).
:- use_module(path_constraints).

:- meta_predicate find_item(3, ?, -).
:- meta_predicate find_items(3, ?).
:- meta_predicate func_path_init(+, 3, 2, ?).

%% TODO: Type and trait search as well
%% -> eg. what types have this documentation or implement these traits
%% TODO: Allow not specifying input / output types (eg. what functions take this / return this)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function level search

%% find_item(+Constraints, ?Item -Score)
% Finds an Item satisfying the constraints, unifying the cost with Cost
find_item(Constraint, Item, Cost) :-
    call(Constraint, Item, Cost, NewConstraint),
    no_constraints_left(NewConstraint).

second((_, B), B).

%% find_item(+Constraints, ?Items -Score)
%% Finds all items satisfying the constraints, and orders them from
% lowest to highest cost.
find_items(Constraint, Items) :-
    setof(
        (Score, Item),
        find_item(Constraint, Item, Score),
        ItemPairs
    ),
    maplist(second, ItemPairs, Items).

%% Finds all functions with the constraints.
func_search(FuncName, Inputs, Outputs, Docs, NameCmp, DocCmp, Funcs) :-
    add_string_constraint(func_field(name), FuncName, NameCmp, no_constraint, C0),
    add_string_constraint(func_field(docs), Docs, DocCmp, C0, C1),
    find_items(
        and_constraint(
            input_constraint(Inputs),
            and_constraint(
                output_constraint(Outputs),
                C1
            )
        ),
        Funcs
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Path search implementations
%% func_path_dfs(+Visited:list, +FnConstraint, +PathConstraint, -Path:list)
% Path is a sequence of functions, which satisfy FuncConstraints
% and its continuations, and in aggregate, satisfy PathConstraint.
test_path(Visited, FnConstraint, PathConstraint, Path) :-
    call(PathConstraint, Visited, none),
    no_constraints_left(FnConstraint),
    reverse(Visited, Path).

func_path_dfs(Visited, FnConstraint, PathConstraint, Path) :-
    test_path(Visited, FnConstraint, PathConstraint, Path).

func_path_dfs(Visited, FnConstraint, PathConstraint, Path) :-
    call(FnConstraint, StartFn, _, NewConstraint),
    call(PathConstraint, Visited, StartFn),
    func_path_dfs([StartFn|Visited], NewConstraint, PathConstraint, Path).

%% Unifies Path with the next shortest path that transforms InputTypes into OutputTypes
%% Helper function for initializing breadth-first search.

% Return suitable candidate
func_path_bfs([(FnConstraint, Visited)|_], PathConstraint, Path) :-
    % If no constraints left, add path to paths
    test_path(Visited, FnConstraint, PathConstraint, Path).

%% Adds all paths which can be reached from the current candidate to the queue,
% and continues bfs iteration.
func_path_bfs([(FnConstraint, Path)|Candidates], PathConstraint, NextPath) :-
    % Add all new paths
    findall((NewConstraint, [StartFn|Path]),
        (
            call(FnConstraint, StartFn, _, NewConstraint),
            call(PathConstraint, Path, StartFn)
        ),
        NewPaths
    ),
    %% No need to turn these into sets, always unique.
    append(Candidates, NewPaths, NewCand),
    func_path_bfs(NewCand, PathConstraint, NextPath).

%% cmp_constraint_list(?Order, @L1, @L2)
% Determine Cmp between L1 and L2 cost/constraint/list triples.
cmp_candidate(Cmp, (Cost1, _, L1), (Cost2, _, L2)) :-
    compare(Cmp, (Cost1, L1), (Cost2, L2)).

func_path_best_fs([(_, FnConstraint, Visited)|_], PathConstraint, Path) :-
    % If no constraints left, add path to paths
    test_path(Visited, FnConstraint, PathConstraint, Path).

% NOTE: additive score does not work with best-first search
% Cost: # of constraints unsat, weighed by importance of constraint,
%       + path length.
% Should increase w/ addition (so path length)
%% Adds all paths which can be reached from the current candidate to the queue,
% and continues bfs iteration.
func_path_best_fs([(OldCost, FnConstraint, Path)|Candidates], PathConstraint, NextPath) :-
    findall((Cost, NewConstraint, [StartFn|Path]),
        (
            call(FnConstraint, StartFn, FuncCost, NewConstraint),
            call(PathConstraint, Path, StartFn),
            Cost is FuncCost + OldCost + 1.0
        ),
        NewPaths
    ),
    %% No need to turn these into sets, always unique.
    append(Candidates, NewPaths, NewCand),
    %% Sort by scores.
    predsort(cmp_candidate, NewCand, SortedCand),
    func_path_best_fs(SortedCand, PathConstraint, NextPath).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exported Path Search
% Easy to use functions for initializing a search with a particular set of parameters.

%% func_path(Strategy, InputTypeList, OutputTypeList, Path:list)
% Path is a sequence of functions, which when applied in sequence,
% accept InputTypeList and produce OutputTypeList. 
% try funcPath(["str"], ["None"], Path). (use ; to get more than one path)
func_path(Strategy, InputTypes, OutputTypes, Path) :-
    func_path_init(
        Strategy,
        and_constraint(
            at_most_n_constraint(999, no_constraint),
            input_constraint(InputTypes)
        ),
        and_constraint([
            output_constraint(OutputTypes)
        ]),
        Path
    ).

%% func_path_init(+Strategy, +FnConstraints, +PathConstraint, -Path:list)
% Unifies the next valid path, using the given strategy, to Path.
func_path_init(dfs, FnConstraint, PathConstraint, Path) :-
    func_path_dfs([], FnConstraint, PathConstraint, Path).
func_path_init(bfs, FnConstraint, PathConstraint, Path) :-
    func_path_bfs([(FnConstraint, [])], PathConstraint, Path).
func_path_init(bestfs, FnConstraint, PathConstraint, Path) :-
    func_path_best_fs([(0.0, FnConstraint, [])], PathConstraint, Path).

%% func_path_no_cycles(+Strategy, ?InputTypeList, ?OutputTypeList, ?Path)
% Path is a sequence of functions, which when applied in sequence,
% accept InputTypeList and produce OutputTypeList. Additionally,
% Path does not contain any cycles.
func_path_no_cycles(Strategy, InputTypes, OutputTypes, Path) :-
    func_path_init(
        Strategy,
        and_constraint(
            at_most_n_constraint(999, no_constraint),
            input_constraint(InputTypes)
        ),
        and_constraint([
            cycle_constraint,
            path_constraints:output_constraint(OutputTypes)
        ]),
        Path
    ).