:- module(parse, [
    parse_signature/6,
    parse_types/3,
    parse_types/4,
    parse_trait/2,
    parse_impls/3,
    format_func/2,
    format_skeleton/6
]).
:- use_module(library(dcg/basics)).

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

%% DCG: Parse a single type
% List<X + Y + Z>
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

% Parse a generic in a function signature:
% X: Add + Sub
single_gen(generic(Name, Bounds)) -->
    ez_str(Name), wh, ":", wh, parse_bounds(Bounds).
single_gen(generic(Name, [])) --> ez_str(Name).

%% DCG: Parse a list of generics
list_of_gen([Gen|Rem]) -->
    single_gen(Gen), wh, ",", wh, list_of_gen(Rem).
list_of_gen([Gen]) --> single_gen(Gen).
list_of_gen([]) --> [].

%% List of bounds: X + Y + Z
parse_bounds([First|Rest]) -->
    single_type(First), wh, "+", wh, parse_bounds(Rest).
parse_bounds([Bound]) -->
    single_type(Bound).
parse_bounds([]) --> [].

%% Trait: X: Add + Sub...
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

parse_impls(String, Type, Impls) :-
    string_codes(String, Codes),
    phrase((ez_str(Type), wh, "impls", wh, parse_bounds(Impls)), Codes), !.
