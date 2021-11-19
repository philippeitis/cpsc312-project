:- module(serde, [
    write_json_metadata/1,
    read_json_metadata/1,
    jsonify_funcs/2
]).
:- use_module(function).
:- use_module(library(http/json)).

%% Helpers for listing items.
get_function(function(Uuid, Name, Generics, Inputs, Outputs, Docs)) :-
    function(Uuid, Name, Generics, Inputs, Outputs, Docs).
get_type(type(Name, Generics, Impls)) :- 
    type(Name, Generics, Impls).
get_trait(trait(Name, Bounds)) :-
    trait(Name, Bounds).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Serializing and deserializing functions, types, and traits
jsonify_generic(generic(Name, Bounds), _{name:Name, bounds:Bounds}) :-
    is_list(Bounds), !.
jsonify_generic(generic(Name, _), _{name:Name}).

jsonify_type(Generic, JsonGeneric) :-
    jsonify_generic(Generic, JsonGeneric), !.
jsonify_type(generic(Name, _), _{name:Name}).
jsonify_type(Type, Type) :- string(Type).
jsonify_type(gen(Type), _{generic:Type}).
jsonify_type(type(Type, SubTypes, Bounds), _{root:Type, generics:JTypes, bounds:Bounds}) :-
    is_list(Bounds),
    jsonify_list_of_types(SubTypes, JTypes), !.
jsonify_type(type(Type, SubTypes, _), _{root:Type, generics:JTypes}) :-
    jsonify_list_of_types(SubTypes, JTypes).
jsonify_type(unbound, _{unbound:true}).

jsonify_trait(trait(Name, Bounds), _{name:Name, bounds:Bounds}).

%% List helpers
jsonify_list_of_generics(Generics, JGenerics) :-
    maplist(jsonify_generic, Generics, JGenerics).

jsonify_list_of_types(Types, JTypes) :-
    maplist(jsonify_type, Types, JTypes).


jsonify_list_of_traits(Traits, JTraits) :-
    maplist(jsonify_trait, Traits, JTraits).

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
%% write_json_metadata(+Stream)
% Writes knowledge-base into the stream in JSON format.
write_json_metadata(Stream) :-
    findall(Func, (
        get_function(Func),
        Func=function(Uuid, _, _, _, _, _),
        \+specialized(_, Uuid)
    ), Functions),
    findall(Type, get_type(Type), Types),
    findall(Trait, get_trait(Trait), Traits),
    write_json_metadata(Stream, Functions, Types, Traits).

%% write_json_metadata(+Stream, +Functions, +Types, +Traits)
%% Writes the provided functions into the stream in JSON format.
write_json_metadata(Stream, Functions, Types, Traits) :-
    jsonify_funcs(Functions, JsonFuncs),
    jsonify_list_of_types(Types, JsonTypes),
    jsonify_list_of_traits(Traits, JsonTraits),
    json_write_dict(Stream,
        _{
            functions:JsonFuncs,
            types:JsonTypes,
            traits: JsonTraits
        }
    ).

%% read_json_metadata(+Stream, -Funcs, -Types, -Traits)
% Loads the functions in the stream into the global knowledge base.
read_json_metadata(Stream) :-
    read_json_metadata(Stream, Funcs, Types, Traits),
    foreach(member(Func, Funcs), assertz(Func)),
    foreach(member(Type, Types), assertz(Type)),
    foreach(member(Trait, Traits), assertz(Trait)).

read_json_metadata(Stream, Funcs, Types, Traits) :-
    json_read_dict(Stream, _{
        functions:JsonFuncs,
        types:JsonTypes,
        traits: JsonTraits
    }),
    jsonify_funcs(Funcs, JsonFuncs),
    jsonify_list_of_types(Types, JsonTypes),
    jsonify_list_of_traits(Traits, JsonTraits).
