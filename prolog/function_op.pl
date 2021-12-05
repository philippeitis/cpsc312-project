:- module(function_op, [
    op(600, xfx, ~),
    '~'/3,
    op(652, xfx, ?~),
    '?~'/3,
    op(666, xfx, #~),
    '#~'/3,
    op(667, xfx, ?#~),
    '?#~'/3
]).

:- use_module(function).
:- use_module(function/serde).

% Actual dark magic - https://swi-prolog.discourse.group/t/block-operator-for-matrix-notation/3506/4

%% Operator guide:
% ~ is the base symbol for getting values from functions
% # is the uuid variant of the operator, which works on uuids instead
% ? is the query symbol, and looks up functions with the key/value pair

%% ~(+Func, +Key, -Value)
% Gets the value with the given key from Func.
'$expand':function(~(_,_), _).
~(function(Uuid, _, _, _, _, _), uuid, Uuid).
~(function(_, Name, _, _, _, _), name, Name).
~(function(_, _, Generics, _, _, _), generics, Generics).
~(function(_, _, _, Inputs, _, _), inputs, Inputs).
~(function(_, _, _, _, Outputs, _), outputs, Outputs).
~(function(_, _, _, _, _, Docs), docs, Docs).

%% ?~(+Key, +Value, -Func)
% Find a function which satisfies the key/value constraint.
'$expand':function(?~(_,_), _).
?~(uuid, Value, Func) :- get_function(Value, Func).
?~(name, Value, Func) :- fname(Uuid, Value), get_function(Uuid, Func).
?~(generics, Value, Func) :- generics(Uuid, Value), get_function(Uuid, Func).
?~(inputs, Value, Func) :- inputs(Uuid, Value), get_function(Uuid, Func).
?~(outputs, Value, Func) :- outputs(Uuid, Value), get_function(Uuid, Func).
?~(docs, Value, Func) :- docs(Uuid, Value), get_function(Uuid, Func).

%% #~(+Key, +Value, -Func)
% Gets the value with the given key from a function with the given UUID.
'$expand':function(#~(_,_), _).
#~(Func, Key, Value) :- func_field(Key, Func, Value).

%% ?#~(+Key, +Value, -Func)
% Find the UUID of a function which satisfies the key/value constraint.
'$expand':function(?#~(_,_), _).
?#~(Key, Value, Func) :- func_field(Key, Func, Value).


%% Example of the value that these operators provide:

% pretty_print_path([]) :- !.
% pretty_print_path([Func]) :-
%     fname(Func, Name),
%     write(Name), !.
% pretty_print_path([Func|Tail]) :-
%     fname(Func, Name),
%     format("~w -> ", [Name]), pretty_print_path(Tail), !.
%
% :- fname(Uuid, "add"), pretty_print_path(Uuid).

pretty_print_path([]) :- !.
pretty_print_path([Func]) :-
    write(Func #~ name), !.
pretty_print_path([Func|Tail]) :-
    format("~w -> ", [Func #~ name]), pretty_print_path(Tail), !.

:- pretty_print_path([name ?#~ "add"]).

%% When refactored, all of the arguments need to be moved, which is an enormous pain, and
% can cause subtle issues which tests might catch, but might also not catch.
% jsonify_fn(
%     function(Uuid, Name, Generics, Inputs, Outputs, Docs),
%     JSON
% ) :-
% var(JSON),
% jsonify_generics(Generics, JGenerics),
% jsonify_types(Inputs, JInputs),
% jsonify_types(Outputs, JOutputs),
% JSON = _{
%     uuid:Uuid,
%     name:Name,
%     generics:JGenerics,
%     inputs:JInputs,
%     outputs:JOutputs,
%     docs:Docs
% }, !.

%% This ~ notation allows us to forgo these positional arguments entirely, in favour of
% just getting the values we need. This makes it much easier to refactor, as we only
% need to change 1 source of truth, not many dozens scattered throughout the codebase
jsonify_fn(
    Fn,
    JSON
) :-
    var(JSON),
    jsonify_generics(Fn ~ generics, JGenerics),
    jsonify_types(Fn ~ inputs, JInputs),
    jsonify_types(Fn ~ outputs, JOutputs),
    JSON = _{
        uuid:Fn ~ uuid,
        name:Fn ~ name,
        generics:JGenerics,
        inputs:JInputs,
        outputs:JOutputs,
        docs:Fn ~ docs
    }.

%% Unfortunately, SWIPL does not currently support functions in calls,
% which would have been very nice to have, and would have allowed doing something like
% call(name ?~ "add", Fn) - which would be very nice for dynamic queries.
% Alternative:
% custom_fn(Key, Value, Fn) :- func_field(Uuid, Fn, Value).
% call(custom_fn(name, "add"), Fn).
% Main issue with the alternative is that it's both more verbose, and obscures the intent
% of the operation.