:- module(path_constraints, [
    output_constraint/3
]).
:- use_module(func_constraints).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Path Constraint Common API
% Args*, Path, Func

%% output_constraint(?OutputTypes:list, +Path:list, +Func)
% Produces true if the last function in Path produces OutputTypes.
output_constraint(OutputTypes, [LastFn|_], none) :-
    output_constraint(OutputTypes, LastFn, _, _).
output_constraint(_, _, Value) :- \+(Value=none).
