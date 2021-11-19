:- module(serde, [
    write_json_metadata/4,
    read_json_metadata/4,
    jsonify_funcs/2
]).
:- use_module(library(http/json)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Serializing and deserializing functions, types, and traits
jsonify_generic(generic(Name, Bounds), _{name:Name, bounds:Bounds}) :-
    is_list(Bounds), !.
jsonify_generic(generic(Name, _), _{name:Name}).

jsonify_type(Generic, JsonGeneric) :-
    jsonify_generic(Generic, JsonGeneric), !.
jsonify_type(Type, Type) :- string(Type), !.
jsonify_type(gen(Type), _{generic:Type}) :- !.
jsonify_type(type(Type, SubTypes, Bounds), _{root:Type, generics:JTypes, bounds:Bounds}) :-
    is_list(Bounds),
    jsonify_types(SubTypes, JTypes), !.
jsonify_type(type(Type, SubTypes, _), _{root:Type, generics:JTypes}) :-
    jsonify_types(SubTypes, JTypes), !.

jsonify_trait(trait(Name, Bounds), _{name:Name, bounds:Bounds}).

%% List helpers
jsonify_generics(Generics, JGenerics) :-
    maplist(jsonify_generic, Generics, JGenerics).

jsonify_types(Types, JTypes) :-
    maplist(jsonify_type, Types, JTypes).

jsonify_traits(Traits, JTraits) :-
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
    jsonify_generics(Generics, JGenerics),
    jsonify_types(Inputs, JInputs),
    jsonify_types(Outputs, JOutputs).

jsonify_funcs(Funcs, JFuncs) :-
    maplist(jsonify_func, Funcs, JFuncs).

%% write_json_metadata(+Stream, +Functions, +Types, +Traits)
%% Writes the provided functions into the stream in JSON format.
write_json_metadata(Stream, Functions, Types, Traits) :-
    jsonify_funcs(Functions, JsonFuncs),
    jsonify_types(Types, JsonTypes),
    jsonify_traits(Traits, JsonTraits),
    json_write_dict(Stream,
        _{
            functions:JsonFuncs,
            types:JsonTypes,
            traits: JsonTraits
        }
    ).

%% read_json_metadata(+Stream, -Funcs, -Types, -Traits)
% Loads the functions in the stream into the global knowledge base.
read_json_metadata(Stream, Funcs, Types, Traits) :-
    json_read_dict(Stream, _{
        functions:JsonFuncs,
        types:JsonTypes,
        traits:JsonTraits
    }),
    jsonify_funcs(Funcs, JsonFuncs),
    jsonify_types(Types, JsonTypes),
    jsonify_traits(Traits, JsonTraits).
