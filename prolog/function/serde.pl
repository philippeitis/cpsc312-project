:- module(serde, [
    write_json_metadata/4,
    read_json_metadata/4,
    jsonify_funcs/2,
    jsonify_type/2,
    jsonify_types/2
]).
:- use_module(library(http/json)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Serializing and deserializing functions, types, and traits
add_impls(JSONIn, List, JSONOut) :-
    is_list(List),
    JSONOut = JSONIn.put(impls, List).
add_impls(JSON, List, JSON) :- \+is_list(List).

add_uuid(JSONIn, Uuid, JSONOut) :-
    \+var(Uuid),
    JSONOut = JSONIn.put(uuid, Uuid).
add_uuid(JSON, Uuid, JSON) :- var(Uuid).

atomize_uuid(Uuid, Uuid) :-
    var(Uuid), !.

atomize_uuid(Uuid, Uuida) :-
    \+var(Uuid), atom_string(Uuid, Uuida), !.

atomize_uuid(Uuid, Uuida) :-
    \+var(Uuida), atom_string(Uuid, Uuida), !.

jsonify_generic(generic(Name, Bounds), _{name:Name, bounds:Bounds}) :-
    is_list(Bounds), !.
jsonify_generic(generic(Name, _), _{name:Name}).

jsonify_type(unbound, _{is_unbound: true}) :- !.
jsonify_type(Generic, JsonGeneric) :-
    jsonify_generic(Generic, JsonGeneric), !.
jsonify_type(Type, Type) :- string(Type), !.
jsonify_type(gen(Name), _{generic:Name}) :- !.
jsonify_type(type(Name, Generics), _{name:Name, generics:JGenerics}) :-
    jsonify_types(Generics, JGenerics), !.

jsonify_type(
        type(Uuid, Name, Generics, Impls, Docs),
        JSON
    ) :-
    var(JSON),
    atomize_uuid(Uuid, Uuida),
    jsonify_generics(Generics, JGenerics),
    JSONNoUuidImpls = _{
        name:Name,
        generics:JGenerics,
        docs:Docs
    },
    add_impls(JSONNoUuidImpls, Impls, JSONNoUuid),
    add_uuid(JSONNoUuid, Uuida, JSON), !.

jsonify_type(
    Type,
    JSON
) :-
    var(Type),
    atomize_uuid(Uuida, JSON.get(uuid, _)),
    jsonify_generics(Generics, JSON.get(generics)),
    Type = type(Uuida, JSON.get(name), Generics, JSON.get(impls, _), JSON.get(docs, "")), !.

jsonify_trait(trait(Name, Bounds), _{name:Name, bounds:Bounds}).

%% List helpers
jsonify_generics(Generics, JGenerics) :-
    maplist(jsonify_generic, Generics, JGenerics).

jsonify_types(Types, JTypes) :-
    maplist(jsonify_type, Types, JTypes).

jsonify_traits(Traits, JTraits) :-
    maplist(jsonify_trait, Traits, JTraits).

jsonify_fn(
        function(Uuid, Name, Generics, Inputs, Outputs, Docs),
        JSON
    ) :-
    var(JSON),
    atomize_uuid(Uuid, Uuida),
    jsonify_generics(Generics, JGenerics),
    jsonify_types(Inputs, JInputs),
    jsonify_types(Outputs, JOutputs),
    JSONNoUuid = _{
        name:Name,
        generics:JGenerics,
        inputs:JInputs,
        outputs:JOutputs,
        docs:Docs
    },
    add_uuid(JSONNoUuid, Uuida, JSON), !.

jsonify_fn(Func, JSON) :-
    var(Func),
    atomize_uuid(Uuida, JSON.get(uuid, _)),
    jsonify_generics(Generics, JSON.get(generics)),
    jsonify_types(Inputs, JSON.get(inputs)),
    jsonify_types(Outputs, JSON.get(outputs)),
    Func = function(Uuida, JSON.get(name), Generics, Inputs, Outputs, JSON.get(docs, "")), !.

jsonify_funcs(Funcs, JFuncs) :-
    maplist(jsonify_fn, Funcs, JFuncs).

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
