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
    specialized/2,
    get_field/3,
    clear_kb/0,
    add_function/6,
    update_type/3,
    update_trait/2,
    get_function/2
]).

:- use_module(library(http/json)).
:- use_module(sequence_ops).
:- dynamic function/6.
:- dynamic trait/2.
:- dynamic type/3.
:- dynamic specialized/2.

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
update_type(Name, NewGenerics, NewImpls) :-
    type(Name, OldGenerics, OldImpls),
    append(OldImpls, NewImpls, AllImpls),
    append(OldGenerics, NewGenerics, AllGenerics),
    % TODO: Add all newly implied impls
    sort(AllImpls, ReducedImpls),
    sort(AllGenerics, ReducedGenerics),
    retract(type(Name, _, _)),
    assertz(type(Name, ReducedGenerics, ReducedImpls)), !.

update_type(Name, NewGenerics, NewImpls) :-
    sort(NewImpls, Impls),
    sort(NewGenerics, Generics),
    assertz(type(Name, Generics, Impls)).

update_trait(Name, NewBounds) :-
    trait(Name, OldBounds),
    append(OldBounds, NewBounds, Bounds),
    % TODO: Add all newly implied impls
    sort(Bounds, ReducedBounds),
    retract(trait(Name, _)),
    assertz(trait(Name, ReducedBounds)), !.

update_trait(Name, NewBounds) :-
    sort(NewBounds, Bounds),
    assertz(trait(Name, Bounds)), !.

add_fn_generic_traits([]) :- !.
add_fn_generic_traits([generic(_, Bounds)|Rest]) :-
    is_list(Bounds),
    foreach(
        member(Bound, Bounds),
        update_trait(Bound, [])
    ),
    add_fn_generic_traits(Rest), !.
add_fn_generic_traits([_|Rest]) :-
    add_fn_generic_traits(Rest), !.

add_function(Uuid, FnName, Generics, InputTypes, OutputTypes, Docs) :-
    uuid(Uuid),
    add_fn_generic_traits(Generics),
    assertz(function(Uuid, FnName, Generics, InputTypes, OutputTypes, Docs)).

get_function(Uuid, function(Uuid, FnName, Generics, InputTypes, OutputTypes, Docs)) :-
    function(Uuid, FnName, Generics, InputTypes, OutputTypes, Docs).

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
:- add_function(_, "listify", [generic("X", _)], [gen("X")], [type("List", [gen("X")], _)], "Produces a list containing the given value.").
:- add_function(_, "sum", [generic("X", ["Add"])], [type("List", [gen("X")], _)], [gen("X")], "Sum, or add sequence adds all of the items in the list. It returns a single value which is the sum of this list.").
:- add_function(_, "add", [generic("X", ["Add"])], [gen("X"), gen("X")], [gen("X")], "Adds two generics").

%% Type processing
%% Produces true if the given type satisfies the constraints of the given generic.
type_is_compat_with_generic(Name, generic(_, GImpls)) :-
    type(Name, _, Impls),
    list_subset(GImpls, Impls).

is_generic(generic(_, _)).

%% Produces true if the key unifies with the generic name and Type satisfies the generic's constraints.
test_interp((Name, Type), generic(Name, GImpls)) :-
    type_is_compat_with_generic(Type, generic(Name, GImpls)).
test_interp((Name, unbound), generic(Name, _)).

%% fn_interp_valid(Func, Interp:list)
% Unifies Interp with an interpration of Func's generics which satisfy all
% type constraints.
fn_interp_valid(Func, Interp) :-
    generics(Func, Generics),
    maplist(test_interp, Interp, Generics),
    sort(Interp, Interp).

%% subst(Interp, GenericTypes, ConcreteTypes)
% Produces true if ConcreteTypes is equivalent to GenericTypes with the given interpretation
subst(Interp, gen(Name), ConcreteType) :-
    member((Name, ConcreteType), Interp), !.
subst(Interp, type(Name, SubTypes, Bounds), type(Name, ConcreteTypes, Bounds)) :-
    maplist(subst(Interp), SubTypes, ConcreteTypes), !.
subst(_, ConcreteType, ConcreteType) :- !.

%% specialize(Func, Interp, Uuid)
% Specializes Func with the given interpretation of the generics, and adds
% the specialized function to the knowledge base. Uuid is unified with the
% uuid of the newly specialized function.
specialize(Func, Interp, Uuid) :-
    function(Func, Name, Generics, Inputs, Outputs, Docs),
    \+(Generics=[]),
    fn_interp_valid(Func, Interp),
    maplist(subst(Interp), Inputs, SpecInputs),
    maplist(subst(Interp), Outputs, SpecOutputs),
    \+function(_, Name, [], SpecInputs, SpecOutputs, Docs),
    uuid(Uuid),
    assertz(function(Uuid, Name, [], SpecInputs, SpecOutputs, Docs)),
    assertz(specialized(Func, Uuid)).
