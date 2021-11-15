:- module(function, [
    function/6,
    type/3,
    trait/2,
    uuid/2,
    fname/2,
    generics/2,
    inputs/2,
    outputs/2,
    docs/2,
    specialize/3,
    get_field/3,
    update_type_trait_impl/2,
    clear_kb/0,
    add_function/6
]).
:- use_module(library(http/json)).
:- dynamic function/6.
:- dynamic trait/2.
:- dynamic type/3.

%% Getters for function.
get_field(Func, name, Field) :- fname(Func, Field).
get_field(Func, docs, Field) :- docs(Func, Field).
get_field(Func, inputs, Field) :- inputs(Func, Field).
get_field(Func, outputs, Field) :- outputs(Func, Field).

uuid(Uuid, Uuid) :- function(Uuid, _, _, _, _, _).
fname(Uuid, Name) :- function(Uuid, Name, _, _, _, _).
inputs(Uuid, Inputs) :- function(Uuid , _, _, Inputs, _, _).
generics(Uuid, Generics) :- function(Uuid, _, Generics, _, _, _).
outputs(Uuid, Outputs) :- function(Uuid, _, _, _, Outputs, _).
docs(Uuid, Documentation) :- function(Uuid, _, _, _, _, Documentation).

clear_kb :-
    retractall(function(_, _, _, _, _, _)),
    retractall(type(_, _, _)),
    retractall(trait(_, _)).

%% Update or add new types and functions
update_type_trait_impl(Name, NewImpls) :-
    type(Name, Bounds, OldImpls),
    append(OldImpls, NewImpls, AllImpls),
    % TODO: Add all newly implied impls
    sort(AllImpls, ReducedImpls),
    retract(type(Name, _, _)),
    assertz(type(Name, Bounds, ReducedImpls)), !.

update_type_trait_impl(Name, NewImpls) :-
    sort(NewImpls, Impls),
    assertz(type(Name, [], Impls)).

try_add_type(Type, Implements) :-
    update_type_trait_impl(Type, Implements).

add_function(Uuid, FnName, Generics, InputTypes, OutputTypes, Docs) :-
    uuid(Uuid),
    assertz(function(Uuid, FnName, Generics, InputTypes, OutputTypes, Docs)).

%% generic(Name, Bounds:list).
%% type(Name, Generics:list, Implements:list)
type("int", [], ["Add"]).
type("Optional", [], []).
type("str", [], []).
type("List", [generic("X", _)], ["Add"]).

%% trait(Name, Bounds:list)
trait("Add", []).

% Function is fnIdentifier, fnInputs, fnOutputs, fnDocs
% fnIdentifier: Currently function name, but we might use an unique identifier
% fnInputs: List of function arguments,
% fnOutputs: List of function outputs,
% fnDocs: User documentation for function
ez_function(Name, Inputs, Outputs, Docs) :-
    add_function(_, Name, [], Inputs, Outputs, Docs).
:- ez_function("parseInt", ["str"], ["int"], "Realises the popular combination of atom_codes/2 and number_codes/2 to convert between atom and number (integer, float or non-integer rational) in one predicate, avoiding the intermediate list. Unlike the ISO standard number_codes/2 predicates, atom_number/2 fails silently in mode (+,-) if Atom does not represent a number.").
:- ez_function("parseInt2",["str"], ["int"], "documentation").
:- ez_function("print", ["int"], ["None"], "Print a term for debugging purposes. The predicate print/1 acts as if defined as below. The print/1 predicate is used primarily through the ~p escape sequence of format/2, which is commonly used in the recipes used by print_message/2 to emit messages. The classical definition of this predicate is equivalent to the ISO predicate write_term/2 using the options portray(true) and numbervars(true). The portray(true) option allows the user to implement application-specific printing of terms printed during debugging to facilitate easy understanding of the output. See also portray/1 and library(portray_text). SWI-Prolog adds quoted(true) to (1) facilitate the copying/pasting of terms that are not affected by portray/1 and to (2) allow numbers, atoms and strings to be more easily distinguished, e.g., 42, '42' and 42.").
:- ez_function("print2", ["int"], ["None"], "documentation").
:- ez_function("increment", ["int"], ["int"], "The increment function, or inc, or incr, will take an integer, or int, and increase its value by 1, or add 1 to it and then return the sum.").
:- ez_function("decrement", ["int"], ["int"], "The decrement function or dec, or decr, will take an integer, or int, and decrease its value by 1, or subtract 1 from it and then return the difference.").
:- add_function(_, "sum", [], [type("List", ["int"], _)], ["int"], "Sum, or add integers, or add ints, will take in a list of integers or ints, and adds up all of the numerical values in the list. It returns a single integer which is the sum of this addition.").
:- add_function(_, "add", [generic("X", ["Add"])], [gen("X"), gen("X")], [gen("X")], "Adds two generics").

%% Type processing
type_is_compat_with_generic(Name, generic(_, GImpls)) :-
    type(Name, _, Impls),
    subset(GImpls, Impls).

is_generic(generic(_, _)).

test_interp((Name, Type), generic(Name, GImpls)) :-
    type_is_compat_with_generic(Type, generic(Name, GImpls)).
test_interp((Name, unbound), generic(Name, _)).

test_interp_against_all([], []).
test_interp_against_all([Interp|RestInterp], [Generic|Rest]) :-
    test_interp(Interp, Generic),
    test_interp_against_all(RestInterp, Rest).

fn_interp_valid(Func, Interp) :-
    generics(Func, Generics),
    test_interp_against_all(Interp, Generics),
    sort(Interp, Interp).

subst(_, [], []).
subst(Interp, [gen(Name)|GenRest], [ConcreteType|Rest]) :-
    member((Name, ConcreteType), Interp),
    subst(Interp, GenRest, Rest), !.
subst(Interp, [ConcreteType|GenRest], [ConcreteType|Rest]) :-
    subst(Interp, GenRest, Rest), !.

specialize(Func, Interp, Uuid) :-
    function(Func, Name, Generics, Inputs, Outputs, Docs),
    \+(Generics=[]),
    fn_interp_valid(Func, Interp),
    subst(Interp, Inputs, SpecInputs),
    subst(Interp, Outputs, SpecOutputs),
    \+function(_, Name, [], SpecInputs, SpecOutputs, Docs),
    uuid(Uuid),
    assertz(function(Uuid, Name, [], SpecInputs, SpecOutputs, Docs)).
