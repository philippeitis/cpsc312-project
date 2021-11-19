:- module(search, [
    func_path/4,
    func_path_no_cycles/4,
    find_fn/3,
    find_funcs/2,
    func_search/7
]).
:- use_module(function).
:- use_module(func_constraints).
:- use_module(path_constraints).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function level search

%% find_fn(-Function, +Constraints, -Score)
% Finds a function satisfying the constraints, unifying the score with Score
find_fn(Constraint, Fn, Score) :-
    call(Constraint, Fn, Score, NewConstraint),
    no_constraints_left(NewConstraint).

%% TODO: Type and trait search as well
%% -> eg. what types have this documentation or implement these traits
%% TODO: Allow not specifying input / output types (eg. what functions take this / return this)

%% Finds all functions satisfying the constraints, and orders them from
% highest score to lowest.
find_funcs(Constraint, Fns) :-
    findall(
        (Score, Fn),
        find_fn(Constraint, Fn, Score),
        FnsUnsorted
    ),
    sort(FnsUnsorted, FnPairs),
    findall(Fn, member((_Score, Fn), FnPairs), Fns).

%% Finds all functions with the constraints.
func_search(FuncName, Inputs, Outputs, Docs, NameCmp, DocCmp, Funcs) :-
    add_field_constraint(name, FuncName, NameCmp, [], C0),
    add_field_constraint(docs, Docs, DocCmp, C0, C1),
    find_funcs(
        and_constraint([
            input_constraint(Inputs)
            |[output_constraint(Outputs)|C1]
        ]),
        Funcs
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Path search implementations
%% func_path_dfs(+Visited:list, +FnConstraint, +PathConstraints, -Path:list)
% Path is a sequence of functions, which satisfy FuncConstraints
% and its continuations, and in aggregate, satisfy PathConstraints.
test_path(Visited, FnConstraint, PathConstraints, Path) :-
    path_constraints(PathConstraints, Visited, none),
    no_constraints_left(FnConstraint),
    reverse(Visited, Path).

func_path_dfs(Visited, FnConstraint, PathConstraints, Path) :-
    test_path(Visited, FnConstraint, PathConstraints, Path).

func_path_dfs(Visited, FnConstraint, PathConstraints, Path) :-
    call(FnConstraint, StartFn, _, NewConstraint),
    path_constraints(PathConstraints, Visited, StartFn),
    func_path_dfs([StartFn|Visited], NewConstraint, PathConstraints, Path).

%% Unifies Path with the next shortest path that transforms InputTypes into OutputTypes
%% Helper function for initializing breadth-first search.

% Return suitable candidate
func_path_bfs([(FnConstraint, Visited)|_], PathConstraints, Path) :-
    % If no constraints left, add path to paths
    test_path(Visited, FnConstraint, PathConstraints, Path).

%% Adds all paths which can be reached from the current candidate to the queue,
% and continues bfs iteration.
func_path_bfs([(FnConstraint, Path)|Candidates], PathConstraints, NextPath) :-
    % Add all new paths
    findall((NewConstraint, [StartFn|Path]),
        (
            call(FnConstraint, StartFn, _, NewConstraint),
            path_constraints(PathConstraints, Path, StartFn)
        ),
        NewPaths
    ),
    %% No need to turn these into sets, always unique.
    append(Candidates, NewPaths, NewCand),
    func_path_bfs(NewCand, PathConstraints, NextPath).

%% cmp_constraint_list(?Order, @L1, @L2)
% Determine Cmp between L1 and L2 cost/constraint/list triples.
cmp_candidate(Cmp, (Cost1, _, L1), (Cost2, _, L2)) :-
    compare(Cmp, (Cost1, L1), (Cost2, L2)).

func_path_best_fs([(_, FnConstraint, Visited)|_], PathConstraints, Path) :-
    % If no constraints left, add path to paths
    test_path(Visited, FnConstraint, PathConstraints, Path).

% NOTE: additive score does not work with best-first search
% Cost: # of constraints unsat, weighed by importance of constraint,
%       + path length.
% Should increase w/ addition (so path length)
%% Adds all paths which can be reached from the current candidate to the queue,
% and continues bfs iteration.
func_path_best_fs([(OldCost, FnConstraint, Path)|Candidates], PathConstraints, NextPath) :-
    findall((Cost, NewConstraint, [StartFn|Path]),
        (
            call(FnConstraint, StartFn, FuncCost, NewConstraint),
            path_constraints(PathConstraints, Path, StartFn),
            Cost is FuncCost + OldCost + 1.0
        ),
        NewPaths
    ),
    %% No need to turn these into sets, always unique.
    append(Candidates, NewPaths, NewCand),
    %% Sort by scores.
    predsort(cmp_candidate, NewCand, SortedCand),
    func_path_best_fs(SortedCand, PathConstraints, NextPath).

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
        and_constraint([input_constraint(InputTypes)]),
        [length_constraint(999),
         path_output_constraint(OutputTypes)],
        Path
    ).

%% func_path_init(Strategy, FnConstraints, PathConstraints, Path:list)
% Unifies the next valid path, using the given strategy, to Path.
func_path_init(dfs, FnConstraint, PathConstraints, Path) :-
    func_path_dfs([], FnConstraint, PathConstraints, Path).
func_path_init(bfs, FnConstraint, PathConstraints, Path) :-
    func_path_bfs([(FnConstraint, [])], PathConstraints, Path).
func_path_init(bestfs, FnConstraint, PathConstraints, Path) :-
    func_path_best_fs([(0.0, FnConstraint, [])], PathConstraints, Path).

%% func_path_no_cycles(?InputTypeList, ?OutputTypeList, ?Path)
% Path is a sequence of functions, which when applied in sequence,
% accept InputTypeList and produce OutputTypeList. Additionally,
% Path does not contain any cycles.
func_path_no_cycles(Strategy, InputTypes, OutputTypes, Path) :-
    func_path_init(
        Strategy,
        and_constraint([input_constraint(InputTypes)]),
        [
            cycle_constraint,
            length_constraint(999),
            path_output_constraint(OutputTypes)
        ],
        Path
    ).