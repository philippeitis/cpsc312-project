:- module(search, [
    func_path/3,
    func_path_no_cycles/3,
    find_func/3,
    find_funcs/2,
    func_search/7
]).
:- use_module(function).
:- use_module(func_constraints).
:- use_module(path_constraints).

% TODO: Make this a breadth-first search which expands highest priority
% items first (eg. items with highest score) - look @ A*.

%% func_path(InputTypeList, OutputTypeList, Path:list)
% Path is a sequence of functions, which when applied in sequence,
% accept InputTypeList and produce OutputTypeList. 
% try funcPath(["str"], ["None"], Path). (use ; to get more than one path)
func_path(InputTypes, OutputTypes, Path) :-
    func_path(
        [],
        [(input_constraint, InputTypes)],
        [(length_constraint, 999),
         (path_output_constraint, OutputTypes)],
        Path
    ).

%% func_path(Visited:list, FuncConstraints, PathConstraints, Path:list)
% Path is a sequence of functions, which satisfy FuncConstraints
% and its continuations, and in aggregate, satisfy PathConstraints.
func_path(Visited, FuncConstraints, PathConstraints, []) :-
    path_constraints(Visited, none, PathConstraints),
    no_constraints_left(FuncConstraints).

func_path(Visited, FuncConstraints, PathConstraints, [StartFn|Rest]) :-
    func_constraints(StartFn, FuncConstraints, _, NewConstraints),
    path_constraints(Visited, StartFn, PathConstraints),
    func_path([StartFn|Visited], NewConstraints, PathConstraints, Rest).

%% func_path_no_cycles(?InputTypeList, ?OutputTypeList, ?Path)
% Path is a sequence of functions, which when applied in sequence,
% accept InputTypeList and produce OutputTypeList. Additionally,
% Path does not contain any cycles.
func_path_no_cycles(InputTypes, OutputTypes, Path) :-
    func_path(
        [],
        [(input_constraint, InputTypes)],
        [(cycle_constraint, _),
        (length_constraint, 999),
        (path_output_constraint, OutputTypes)], Path).

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
    sort(0, @>, FuncsUnsorted, FuncPairs),
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
