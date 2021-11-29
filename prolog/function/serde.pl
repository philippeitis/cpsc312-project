:- module(serde, [
    write_json_metadata/4,
    read_json_metadata/4,
    jsonify_funcs/2,
    jsonify_type/2
]).
:- use_module(library(http/json)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Serializing and deserializing functions, types, and traits
jsonify_generic(generic(Name, Impls), _{name:Name, impls:Impls}) :-
    is_list(Impls), !.
jsonify_generic(generic(Name, _), _{name:Name}).

jsonify_type(Generic, JsonGeneric) :-
    jsonify_generic(Generic, JsonGeneric), !.
jsonify_type(Type, Type) :- string(Type), !.
jsonify_type(gen(Name), _{generic:Name}) :- !.
jsonify_type(type(Name, Generics, Impls), _{name:Name, generics:JGenerics, impls:Impls}) :-
    is_list(Impls),
    jsonify_types(Generics, JGenerics), !.
jsonify_type(type(Name, Generics, _), _{name:Name, generics:JGenerics}) :-
    jsonify_types(Generics, JGenerics), !.
jsonify_trait(trait(Name, Bounds), _{name:Name, bounds:Bounds}).

%% List helpers
jsonify_generics(Generics, JGenerics) :-
    maplist(jsonify_generic, Generics, JGenerics).

jsonify_types(Types, JTypes) :-
    maplist(jsonify_type, Types, JTypes).

jsonify_traits(Traits, JTraits) :-
    maplist(jsonify_trait, Traits, JTraits).

jsonify_func(
    function(Uuid, Name, Generics, Inputs, Outputs, ""),
    _{
        uuid:Uuida,
        name:Name,
        generics:JGenerics,
        inputs:JInputs,
        outputs:JOutputs
    }) :-
    (\+var(Uuid); \+var(Uuida)),
    atom_string(Uuid, Uuida),
    jsonify_generics(Generics, JGenerics),
    jsonify_types(Inputs, JInputs),
    jsonify_types(Outputs, JOutputs), !.

jsonify_func(
    function(_, Name, Generics, Inputs, Outputs, ""),
    _{
        name:Name,
        generics:JGenerics,
        inputs:JInputs,
        outputs:JOutputs
    }) :-
    jsonify_generics(Generics, JGenerics),
    jsonify_types(Inputs, JInputs),
    jsonify_types(Outputs, JOutputs), !.

jsonify_func(
    function(Uuid, Name, Generics, Inputs, Outputs, Docs),
    _{
        uuid:Uuida,
        name:Name,
        generics:JGenerics,
        inputs:JInputs,
        outputs:JOutputs,
        docs:Docs
    }) :-
    (\+var(Uuid); \+var(Uuida)),
    atom_string(Uuid, Uuida),
    jsonify_generics(Generics, JGenerics),
    jsonify_types(Inputs, JInputs),
    jsonify_types(Outputs, JOutputs), !.

jsonify_func(
    function(_, Name, Generics, Inputs, Outputs, Docs),
    _{
        name:Name,
        generics:JGenerics,
        inputs:JInputs,
        outputs:JOutputs,
        docs:Docs
    }) :-
    jsonify_generics(Generics, JGenerics),
    jsonify_types(Inputs, JInputs),
    jsonify_types(Outputs, JOutputs), !.

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
