:- module(function, [
    function/4,
    name/2,
    inputs/2,
    outputs/2,
    docs/2,
    write_json_funcs/1,
    read_json_funcs/1
]).
:- use_module(library(http/json)).
:- dynamic function/4.

type("int").
type("None").

% Function is fnIdentifier, fnInputs, fnOutputs, fnDocs
% fnIdentifier: Currently function name, but we might use an unique identifier
% fnInputs: List of function arguments,
% fnOutputs: List of function outputs,
% fnDocs: User documentation for function
function("parseInt", ["str"], ["int"], "Realises the popular combination of atom_codes/2 and number_codes/2 to convert between atom and number (integer, float or non-integer rational) in one predicate, avoiding the intermediate list. Unlike the ISO standard number_codes/2 predicates, atom_number/2 fails silently in mode (+,-) if Atom does not represent a number.").
function("parseInt2", ["str"], ["int"],"documentation").
function("print", ["int"], ["None"], "Print a term for debugging purposes. The predicate print/1 acts as if defined as below. The print/1 predicate is used primarily through the ~p escape sequence of format/2, which is commonly used in the recipes used by print_message/2 to emit messages. The classical definition of this predicate is equivalent to the ISO predicate write_term/2 using the options portray(true) and numbervars(true). The portray(true) option allows the user to implement application-specific printing of terms printed during debugging to facilitate easy understanding of the output. See also portray/1 and library(portray_text). SWI-Prolog adds quoted(true) to (1) facilitate the copying/pasting of terms that are not affected by portray/1 and to (2) allow numbers, atoms and strings to be more easily distinguished, e.g., 42, '42' and 42.").
function("print2", ["int"], ["None"], "documentation").
function("increment", ["int"], ["int"], "The increment function, or inc, or incr, will take an integer, or int, and increase its value by 1, or add 1 to it and then return the sum.").
function("decrement", ["int"], ["int"], "The decrement function or dec, or decr, will take an integer, or int, and decrease its value by 1, or subtract 1 from it and then return the difference.").
function("sum", ["List[int]"], ["int"], "Sum, or add integers, or add ints, will take in a list of integers or ints, and adds up all of the numerical values in the list. It returns a single integer which is the sum of this addition.").

name(Func, Name) :- function(Func, _, _, _), Name=Func.
inputs(Func, Inputs) :- function(Func, Inputs, _, _).
outputs(Func, Outputs) :- function(Func, _, Outputs, _).
docs(Func, Documentation) :- function(Func, _, _, Documentation).

write_json_funcs(Stream) :-
    findall(_{
        name:Name,
        inputs:Inputs,
        outputs:Outputs,
        docs:Docs
    }, function(Name, Inputs, Outputs, Docs), Functions),
    write_json_funcs(Stream, Functions).

write_json_funcs(Stream, Functions) :-
    json_write_dict(Stream, _{functions:Functions}).

read_json_funcs(Stream) :-
    json_read_dict(Stream, _{
        functions:JsonFuncs
    }),
    foreach(
        member(_{name:Name, inputs:Inputs, outputs:Outputs, docs:Docs}, JsonFuncs),
        assertz(function(Name, Inputs, Outputs, Docs))
    ).