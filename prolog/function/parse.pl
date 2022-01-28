:- module(parse, [
    parse_signature/6,
    parse_types/3,
    parse_types/4,
    parse_trait/2,
    parse_type/2,
    format_skeleton/6,
    format_signature/2
]).
:- use_module(function).
:- use_module(library(dcg/basics)).
:- use_module(sequence_ops).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subst_generics_into(
    Generics,
    type(TName, TypeUGenerics, TBounds),
    type(TName, TypeGenerics, TBounds)
) :-
    subst_generics_into(Generics, TypeUGenerics, TypeGenerics), !.
subst_generics_into(Generics, Name, gen(Name)) :-
    member(generic(Name, _), Generics), !.
subst_generics_into(_, _, _) :- !.

subst_generics([], Types, Types) :- !.
subst_generics(Generics, UTypes, Types) :-
    maplist(subst_generics_into(Generics), UTypes, Types), !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DCG Helpers
ez_str(String) -->
    string(StringCodes),
    {string_codes(String, StringCodes)}.

%% DCG: Parse whitespace
wh -->  " ", wh.
wh -->  "".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parsing text representation of functions

%% DCG: Parse optional documentation
optional_doc(Docs) -->
    wh, "|", wh, ez_str(Docs).
optional_doc("") --> wh.

%% DCG: Parse a single type
% List<X, Y, Z>
single_type(type(Name, Generics, _)) -->
    ez_str(Name),
    wh, "<", wh, 
    list_of_types(Generics),
    wh, ">", wh.
single_type(Name) --> ez_str(Name).

%% DCG: Parse a list of types
% int, str, ...
list_of_types([Type|Rem]) -->
    single_type(Type), wh, ",", wh, list_of_types(Rem).
list_of_types([Type]) --> single_type(Type).
list_of_types([]) --> [].

%% DCG: Parse a single type declaration
% List<X: ..., Y, Z> :
type_decl(type(_, Name, Generics, Decl, Docs)) -->
    "type", wh,
    ez_str(Name),
    wh,
    list_of_gen(Generics),
    wh, ":", wh,
    parse_bounds(Decl),
    wh,
    optional_doc(Docs).

type_decl(type(Name, Generics, [], Docs)) -->
    "type", wh,
    ez_str(Name), wh,
    list_of_gen(Generics),
    wh,
    optional_doc(Docs).

% Parse a generic in a function signature:
% X: Add + Sub
single_gen(generic(Name, Bounds)) -->
    ez_str(Name), wh, ":", wh, parse_bounds(Bounds).
single_gen(generic(Name, [])) --> ez_str(Name).

%% DCG: Parse a list of generics
list_of_gen_([Gen|Rem]) -->
    single_gen(Gen), wh, ",", wh, list_of_gen_(Rem).
list_of_gen_([Gen]) --> single_gen(Gen).
list_of_gen_([]) --> [].

list_of_gen(Gen) --> "<", wh, list_of_gen_(Gen), wh, ">".
list_of_gen([]) --> "".

%% List of bounds: X + Y + Z
parse_bounds([First|Rest]) -->
    single_type(First), wh, "+", wh, parse_bounds(Rest).
parse_bounds([Bound]) -->
    single_type(Bound).
parse_bounds([]) --> [].

%% Trait: trait X: Add + Sub...
parse_trait_(trait(Name, Bounds)) -->
    "trait", " ", wh, ez_str(Name), wh,
    ":", wh, parse_bounds(Bounds).
parse_trait_(trait(Name, [])) --> ez_str(Name).

%% DCG: Parse the function type signature
parse_type_sig(Inputs, Outputs) -->
    "[", list_of_types(Inputs), "]",
    wh, "->", wh,
    "[", list_of_types(Outputs), "]".

%% DCG: Parse a function signature.
parse_sig(Name, Generics, Inputs, Outputs, Docs) -->
    ez_str(Name), wh, list_of_gen(Generics),
    wh, "::", wh,
    parse_type_sig(Inputs, Outputs),
    optional_doc(Docs).
parse_sig(Name, [], Inputs, Outputs, Docs) -->
    ez_str(Name),
    wh, "::", wh,
    parse_type_sig(Inputs, Outputs),
    optional_doc(Docs).

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

parse_type(String, Type) :-
    string_codes(String, Codes),
    phrase(type_decl(Type), Codes), !.

%% Parse a function signature in roughly Haskell format
parse_signature(String, Name, Generics, Inputs, Outputs, Docs) :-
    string_codes(String, Codes),
    phrase(parse_sig(Name, Generics, UInputs, UOutputs, Docs), Codes),
    subst_generics(Generics, UInputs, Inputs),
    subst_generics(Generics, UOutputs, Outputs), !.

%% Formats the function skeleton.
format_skeleton(String, Name, [], Inputs, Outputs, Docs) :-
    format(string(String), '~w :: ~w -> ~w | ~w', [Name, Inputs, Outputs, Docs]).
format_skeleton(String, Name, Generics, Inputs, Outputs, Docs) :-
    format(string(String), '~w<~w> :: ~w -> ~w | ~w', [Name, Generics, Inputs, Outputs, Docs]).

format_generic(generic(Name, Bounds), String) :-
    \+is_list(Bounds),
    format(string(String), "~w", Name), !.

format_generic(generic(Name, Bounds), String) :-
    is_list(Bounds),
    join(Bounds, " + ", SubStr),
    format(string(String), "~w: ~w", [Name, SubStr]), !.

format_generic(G, String) :-
    format(string(String), "~w", [G]), !.

format_generics([], "") :- !.
format_generics(Generics, String) :-
    maplist(format_generic, Generics, GenericSubs),
    join(GenericSubs, ", ", GenericContents),
    format(string(String), "<~w>", GenericContents).

format_type(type(Name, SubTypes), String) :-
    is_list(SubTypes),
    SubTypes = [_|_],
    maplist(format_type, SubTypes, SubTys),
    join(SubTys, ", ", Tys),
    format(string(String), "~w<~w>", [Name, Tys]), !.

format_type(type(Name, _), String) :-
    format(string(String), "~w", [Name]).

format_type(gen(Ty), String) :-
    format(string(String), "~w", Ty).


format_type(Ty, String) :-
    format(string(String), "~w", Ty).

format_signature(function(_, _, Generics, Inputs, Outputs, _), String) :-
    format_generics(Generics, GenString),
    maplist(format_type, Inputs, Inputsf),
    join(Inputsf, ", ", Inputsff),
    maplist(format_type, Outputs, Outputsf),
    join(Outputsf, ", ", Outputsff),
    format(string(String), '~w :: [~w] -> [~w]', [GenString, Inputsff, Outputsff]).
