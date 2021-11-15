:- module(serde, [
    write_json_metadata/1,
    read_json_metadata/1
]).
:- use_module(function).
:- use_module(library(http/json)).

%% Helper for listing all functions.
get_function(function(Uuid, Name, Generics, Inputs, Outputs, Docs)) :-
    function(Uuid, Name, Generics, Inputs, Outputs, Docs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Serializing and deserializing JSON
jsonify_generic(generic(Name, Bounds), _{name:Name, bounds:Bounds}).
jsonify_type(Type, Type) :- string(Type).
jsonify_type(gen(Type), _{generic:Type}).
jsonify_type(unbound, _{unbound:true}).

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
    write_json_metadata(Stream, Functions, _Types, _Traits).

%% Writes the provided functions into the stream in JSON format.
write_json_metadata(Stream, Functions, _Types, _Traits) :-
    jsonify_funcs(Functions, JsonFuncs),
    json_write_dict(Stream, _{functions:JsonFuncs}).

%% Loads the functions in the stream into the global knowledge base.
read_json_metadata(Stream) :-
    json_read_dict(Stream, _{
        functions:JsonFuncs
    }),
    jsonify_funcs(Funcs, JsonFuncs),
    foreach(member(Func, Funcs), assertz(Func)).
