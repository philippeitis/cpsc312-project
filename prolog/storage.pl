:- module(storage, [
    find_all_functions/1,
    find_all_types/1,
    find_all_traits/1,
    load_knowledge_base/1,
    store_knowledge_base/1,
    clear_knowledge_base/0
]).

:- use_module(function).
:- use_module(function/serde).

find_all_functions(Functions) :-
    findall(Func, (
        get_function(Func),
        Func=function(Uuid, _, _, _, _, _),
        \+specialized(_, Uuid)
    ), Functions).

find_all_types(Types) :-
    findall(Type, get_type(Type), Types).

find_all_traits(Traits) :-
    findall(Trait, get_trait(Trait), Traits).

%% Helpers for listing items.
get_function(function(Uuid, Name, Generics, Inputs, Outputs, Docs)) :-
    function(Uuid, Name, Generics, Inputs, Outputs, Docs).
get_type(type(Name, Generics, Impls)) :- 
    type(Name, Generics, Impls).
get_trait(trait(Name, Bounds)) :-
    trait(Name, Bounds).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% JSON persistence

%% load_knowledge_base(+Stream)
% Writes knowledge-base into the stream in JSON format.
load_knowledge_base(Stream) :-
    read_json_metadata(Stream, Funcs, Types, Traits),
    foreach(member(Func, Funcs), assertz(Func)),
    foreach(member(Type, Types), assertz(Type)),
    foreach(member(Trait, Traits), assertz(Trait)).

%% store_knowledge_base(+Stream)
% Writes knowledge-base into the stream in JSON format.
store_knowledge_base(Stream) :-
    find_all_functions(Functions),
    find_all_types(Types),
    find_all_traits(Traits),
    write_json_metadata(Stream, Functions, Types, Traits).

%% clear_knowledge_base
% Clears all items from the knowledge base
clear_knowledge_base :-
    retractall(function(_, _, _, _, _, _)),
    retractall(type(_, _, _)),
    retractall(trait(_, _)).