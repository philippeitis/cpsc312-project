:- module(search, [
    func_path/3,
    func_path_no_cycles/3,
    find_func/3,
    find_funcs/2
]).
:- use_module(function).
:- use_module(constraint).

%% func_path(?InputTypeList, ?OutputTypeList, ?Path)
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

% TODO: Make this a breadth-first search which expands highest priority
% items first (eg. items with highest score) - look @ A*.
% TODO: Add path length constraint and/or threshold to this.

%% func_path(?Visited, ?FuncConstraints, ?PathConstraints, ?Path)
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

%% find_func(Function, Constraints, )
find_func(Func, FuncConstraints, Score) :-
    func_constraints(Func, FuncConstraints, Score, NewConstraints),
    no_constraints_left(NewConstraints).

find_funcs(Funcs, FuncConstraints) :-
    findall(
        (Score, Func),
        find_func(Func, FuncConstraints, Score),
        FuncsUnsorted
    ),
    sort(0, @>, FuncsUnsorted, FuncPairs),
    findall(Func, member((_Score, Func), FuncPairs), Funcs).
    
