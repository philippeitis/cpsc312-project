:- module(function, [
    function/6,
    uuid/2,
    fname/2,
    generics/2,
    inputs/2,
    outputs/2,
    docs/2,
    specialize/3,
    get_field/3,
    write_json_metadata/1,
    read_json_metadata/1,
    parse_signature/6,
    parse_types/3,
    parse_types/4,
    format_func/2,
    format_skeleton/6,
    clear_funcs/0,
    add_function/6
]).
:- use_module(library(dcg/basics)).
:- use_module(library(http/json)).
:- dynamic function/6.
:- dynamic trait/2.
:- dynamic type/3.

list_subset([], _).
list_subset([First|Rest], B) :-
    member(First, B),
    list_subset(Rest, B), !.

clear_funcs :- retractall(function(_, _, _, _, _, _)).

%% Update or add new types and functions
update_type_trait_impl(Name, NewImpls) :-
    type(Name, Bounds, OldImpls),
    append(OldImpls, NewImpls, AllImpls),
    % TODO: Add all newly implied impls
    sort(AllImpls, ReducedImpls),
    retract(type(Name, _, _)),
    assertz(type(Name, Bounds, ReducedImpls)), !.

update_type_trait_impl(Name, NewImpls) :-
    sort(NewImpls, ImplS),
    assertz(type(Name, [], ImplS)).

try_add_type(Type, Implements) :-
    update_type_trait_impl(Type, Implements).

add_function(Uuid, FnName, Generics, InputTypes, OutputTypes, Docs) :-
    uuid(Uuid),
    assertz(function(Uuid, FnName, Generics, InputTypes, OutputTypes, Docs)).

%% generic(Name, Bounds:list).
%% type(Name, Generics:list, Implements:list)
type("int", [], ["Add"]).
type("Optional", [], []).
type("str", [], ["Add"]).
type("List", [generic(_, _)], ["Add"]).

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
% :- add_function(_, "sum", [generic("X")], [type("List", ["int"], _)], ["int"], "Sum, or add integers, or add ints, will take in a list of integers or ints, and adds up all of the numerical values in the list. It returns a single integer which is the sum of this addition.").
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

%% Helper for listing all functions.
get_function(function(Uuid, Name, Generics, Inputs, Outputs, Docs)) :-
    function(Uuid, Name, Generics, Inputs, Outputs, Docs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Taking data to and from JSON:
jsonify_generic(generic(Name, Bounds), _{name:Name, bounds:Bounds}).
jsonify_type(Type, Type) :- string(Type).
jsonify_type(gen(Type), _{generic:Type}).

jsonify_list_of_generics([], []).
jsonify_list_of_generics([Generic|Generics], [JGeneric, JGenerics]) :-
    jsonify_generic(Generic, JGeneric),
    jsonify_list_of_generics(Generics, JGenerics).

jsonify_list_of_types([], []).
jsonify_list_of_types([Type|Types], [JType|JTypes]) :-
    jsonify_type(Type, JType),
    jsonify_list_of_types(Types, JTypes).

jsonify_func(
    function(Uuid, Name, Generics, Inputs, Outputs, Docs),
    _{
        uuid:Uuid,
        name:Name,
        generics:JGenerics,
        inputs:JInputs,
        outputs:JOutputs,
        docs:Docs
    }) :-
    jsonify_list_of_generics(Generics, JGenerics),
    jsonify_list_of_types(Inputs, JInputs),
    jsonify_list_of_types(Outputs, JOutputs).

jsonify_funcs([], []).
jsonify_funcs([Func|Funcs], [JFunc|JFuncs]) :-
    jsonify_func(Func, JFunc),
    jsonify_funcs(Funcs, JFuncs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% JSON persistence
%% Writes knowledge-base into the stream in JSON format.
write_json_metadata(Stream) :-
    findall(Func, get_function(Func), Functions),
    % findall(_{
    %     name:Name,
    %     generics:Generics,
    %     impls:Impls
    % }, type(Name, Generics, Impls), Types),
    % findall(_{
    %     name:Name,
    %     bounds: Bounds
    % }, trait(Name, Bounds), Traits),
    write_json_metadata(Stream, Functions, Types, Traits).

%% Writes the provided functions into the stream in JSON format.
write_json_metadata(Stream, Functions, Types, Traits) :-
    jsonify_funcs(Functions, JsonFuncs),
    json_write_dict(Stream, _{functions:JsonFuncs}).

%% Loads the functions in the stream into the global knowledge base.
read_json_metadata(Stream) :-
    json_read_dict(Stream, _{
        functions:JsonFuncs
    }),
    jsonify_funcs(Funcs, JsonFuncs),
    foreach(member(Func, Funcs), assertz(Func)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parsing text representation of functions
ez_str(String) -->
    string(StringCodes),
    {string_codes(String, StringCodes)}.

%% DCG: Parse whitespace
wh -->  " ", wh.
wh -->  "".

%% DCG: Parse a single type
single_type(type(Name, Generics, _)) -->
    ez_str(Name),
    wh, "<", wh, 
    list_of_types(Generics),
    wh, ">", wh.
single_type(Name) --> ez_str(Name).

%% DCG: Parse a list of types
list_of_types([Type|Rem]) -->
    single_type(Type), wh, ",", wh, list_of_types(Rem).
list_of_types([Type]) --> single_type(Type).
list_of_types([]) --> [].

single_gen(generic(Name, Bounds)) -->
    ez_str(Name), wh, ":", wh, parse_bounds(Bounds).
single_gen(generic(Name, [])) --> ez_str(Name).

%% DCG: Parse a list of types
list_of_gen([Gen|Rem]) -->
    single_gen(Gen), wh, ",", wh, list_of_gen(Rem).
list_of_gen([Gen]) --> single_gen(Gen).
list_of_gen([]) --> [].

parse_bounds([First|Rest]) -->
    single_type(First), wh, "+", wh, parse_bounds(Rest).
parse_bounds([Bound]) -->
    single_type(Bound).
parse_bounds([]) --> [].

parse_trait_(trait(Name, Bounds)) -->
    ez_str(Name), wh,
    ":", wh, parse_bounds(Bounds).
parse_trait_(trait(Name, [])) --> ez_str(Name).

%% DCG: Parse the function type signature
parse_type_sig(Inputs, Outputs) -->
    "[", list_of_types(Inputs), "]",
    wh, "->", wh,
    "[", list_of_types(Outputs), "]".

%% DCG: Parse a function signature.
parse_sig(Name, Generics, Inputs, Outputs, Docs) -->
    ez_str(Name), wh, "<", list_of_gen(Generics), wh, ">",
    wh, "::", wh,
    parse_type_sig(Inputs, Outputs),
    optional_doc(Docs).
parse_sig(Name, [], Inputs, Outputs, Docs) -->
    ez_str(Name),
    wh, "::", wh,
    parse_type_sig(Inputs, Outputs),
    optional_doc(Docs).

%% DCG: Parse optional documentation
optional_doc(Docs) -->
    wh, "|", wh, ez_str(Docs).
optional_doc("") --> wh.

%% Parse a type signature.
parse_types(String, Inputs, Outputs) :-
    parse_types(String, Inputs, Outputs, _).

%% Parse a type signature from String and return the remainder in Rest.
parse_types(String, Inputs, Outputs, Rest) :-
    string_codes(String, Codes),
    phrase(parse_type_sig(Inputs, Outputs), Codes, RestCodes),
    string_codes(Rest, RestCodes).

parse_trait(String, Trait) :-
    string_codes(String, Codes),
    phrase(parse_trait_(Trait), Codes), !.

%% Parse a function signature in roughly Haskell format
parse_signature(String, Name, Generics, Inputs, Outputs, Docs) :-
    string_codes(String, Codes),
    phrase(parse_sig(Name, Generics, Inputs, Outputs, Docs), Codes), !.

%% Formats the function with the given name
format_func(String, Uuid) :-
    function(Uuid, Name, Generics, Inputs, Outputs, Docs),
    format_skeleton(String, Name, Generics, Inputs, Outputs, Docs).

%% Formats the function skeleton.
format_skeleton(String, Name, [], Inputs, Outputs, Docs) :-
    format(string(String), '~w :: ~w -> ~w | ~w', [Name, Inputs, Outputs, Docs]).
format_skeleton(String, Name, Generics, Inputs, Outputs, Docs) :-
    format(string(String), '~w<~w> :: ~w -> ~w | ~w', [Name, Generics, Inputs, Outputs, Docs]).