:- module(search, [
    func_path/4,
    func_path_no_cycles/4,
    find_func/3,
    find_funcs/2,
    func_search/7
]).
:- use_module(function).
:- use_module(func_constraints).
:- use_module(path_constraints).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Function level search

%% find_func(Function, Constraints, Score)
% Finds a function satisfying the constraints, unifying the score with Score
find_func(Func, FuncConstraints, Score) :-
    func_constraints(Func, FuncConstraints, Score, NewConstraints),
    no_constraints_left(NewConstraints).

%% Finds all functions satisfying the constraints, and orders them from
% highest score to lowest.
find_funcs(Funcs, FuncConstraints) :-
    findall(
        (Score, Func),
        find_func(Func, FuncConstraints, Score),
        FuncsUnsorted
    ),
    sort(FuncsUnsorted, FuncPairs),
    findall(Func, member((_Score, Func), FuncPairs), Funcs).

%% Finds all functions with the constraints.
func_search(FuncName, Inputs, Outputs, Docs, NameCmp, DocCmp, Funcs) :-
    add_field_constraint(name, FuncName, NameCmp, [], C0),
    add_field_constraint(docs, Docs, DocCmp, C0, C1),
    find_funcs(
        Funcs,
        [
            (input_constraint, Inputs)
            |[(output_constraint, Outputs)|C1]
        ]
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Path search implementations
%% func_path_dfs(Visited:list, FuncConstraints, PathConstraints, Path:list)
% Path is a sequence of functions, which satisfy FuncConstraints
% and its continuations, and in aggregate, satisfy PathConstraints.
func_path_dfs(Visited, FnConstraints, PathConstraints, Path) :-
    path_constraints(Visited, none, PathConstraints),
    no_constraints_left(FnConstraints),
    reverse(Visited, Path).

func_path_dfs(Visited, FnConstraints, PathConstraints, Path) :-
    func_constraints(StartFn, FnConstraints, _, NewConstraints),
    path_constraints(Visited, StartFn, PathConstraints),
    func_path_dfs([StartFn|Visited], NewConstraints, PathConstraints, Path).

%% Unifies Path with the next shortest path that transforms InputTypes into OutputTypes
%% Helper function for initializing breadth-first search.

func_path_bfs([], _, _) :- fail.

% Return suitable candidate
func_path_bfs([(FnConstraints, RPath)|_], PathConstraints, Path) :-
    % If no constraints left, add path to paths
    no_constraints_left(FnConstraints),    
    path_constraints(RPath, none, PathConstraints),
    reverse(RPath, Path).

%% Adds all paths which can be reached from the current candidate to the queue,
% and continues bfs iteration.
func_path_bfs([(FnConstraints, Path)|Candidates], PathConstraints, NextPath) :-
    % Add all new paths
    findall((NewConstraints, [StartFn|Path]),
        ( 
            func_constraints(StartFn, FnConstraints, _, NewConstraints),
            path_constraints(Path, StartFn, PathConstraints)
        ),
        NewPaths
    ),
    %% No need to turn these into sets, always unique.
    append(Candidates, NewPaths, NewCand),
    func_path_bfs(NewCand, PathConstraints, NextPath).

%% cmp_constraint_list(?Order, @L1, @L2)
% Determine Cmp between L1 and L2 cost/constraint/list triples.
cmp_candidate(Cmp, (Cost1, _, L1), (Cost2, _, L2)) :-
    length(L1, Len1), length(L2, Len2),
    compare(Cmp, (Cost1, Len1, L2), (Cost2, Len2, L1)).

func_path_best_fs([], _, _) :- fail.

func_path_best_fs([(_, FnConstraints, Path)|_], PathConstraints, Path) :-
    % If no constraints left, add path to paths
    no_constraints_left(FnConstraints),
    path_constraints(Path, none, PathConstraints).

% NOTE: additive score does not work with best-first search
% Cost: # of constraints unsat, weighed by importance of constraint,
%       + path length.
% Should increase w/ addition (so path length)
%% Adds all paths which can be reached from the current candidate to the queue,
% and continues bfs iteration.
func_path_best_fs([(OldCost, FnConstraints, Path)|Candidates], PathConstraints, NextPath) :-
    findall((Cost, NewConstraints, [StartFn|Path]),
        ( 
            func_constraints(StartFn, FnConstraints, FuncCost, NewConstraints),
            path_constraints(Path, StartFn, PathConstraints),
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
        [(input_constraint, InputTypes)],
        [(length_constraint, 999),
         (path_output_constraint, OutputTypes)],
        Path
    ).

%% func_path_init(Strategy, FnConstraints, PathConstraints, Path:list)
% Unifies the next valid path, using the given strategy, to Path.
func_path_init(dfs, FnConstraints, PathConstraints, Path) :-
    func_path_dfs([], FnConstraints, PathConstraints, Path).
func_path_init(bfs, FnConstraints, PathConstraints, Path) :-
    func_path_bfs([(FnConstraints, [])], PathConstraints, Path).
func_path_init(bestfs, FnConstraints, PathConstraints, Path) :-
    func_path_best_fs([(0.0, FnConstraints, [])], PathConstraints, Path).

%% func_path_no_cycles(?InputTypeList, ?OutputTypeList, ?Path)
% Path is a sequence of functions, which when applied in sequence,
% accept InputTypeList and produce OutputTypeList. Additionally,
% Path does not contain any cycles.
func_path_no_cycles(Strategy, InputTypes, OutputTypes, Path) :-
    func_path_init(
        Strategy,
        [(input_constraint, InputTypes)],
        [(cycle_constraint, _),
        (length_constraint, 999),
        (path_output_constraint, OutputTypes)],
        Path
    ).
