# Overview
This project provides a REPL, which can be initialized by running `swipl main.pl`:
```console
user:~/cpsc312-project/prolog$ swipl main.pl
>>> 
```

Use `swipl main.pl --help` for help information:
```console
user:~/cpsc312-project/prolog$ swipl main.pl --help
Use `help command` for help with a particular command
Available commands: 
    define
    clear
    search
    path
    store
    load
    launch
    quit
    os
    setup
```

and `swipl main.pl --help COMMAND` for instructions for a particular command:
```console
user:~/cpsc312-project/prolog$ swipl main.pl --help define
Defines a function from user input.
Example: define fnName :: [arg1, arg2] -> [output1, output2] | doc
```

## Examples

In this section, we go over the most important commands in the FastFunc CLI interface, which cover the core elements our POC hopes to demonstrate. This includes defining functions, searching for them, and being able to use generic types and functions.

The primary path composition and search functionality has settings which can be set using `--KEY=VALUE` style arguments, and accepts the same syntax for defining function signatures as described in the MVP, though function names and documentation are omitted, as these are optional, which can be seen in the following examples.

If you want to find at most 3 function paths, which accept an `int`, and produce an `int`:
```console
user:~/cpsc312-project/prolog$ swipl main.pl
>>> path [int] -> [int] --limit=3
Found 3 solutions:
increment
decrement
add
```

If you want to find a function whose name contains the subsequence `pant2` (other keys and options are described at [CLI, REST API Parameters](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project#cli-rest-api-parameters)):
```console
user:~/cpsc312-project/prolog$ swipl main.pl
>>> search [str] -> [int] --name=pant2 --name_cmp=subseq
Found 1 solutions:
Function: parseInt2
```

You can define new functions, and then search for them:
```console
user:~/cpsc312-project/prolog$ swipl main.pl
>>> define pow :: [int, int] -> [int] | Raises x to the power of e
Adding function: pow
>>> search [int] -> [int] --docs=power --doc_cmp=substr
Found 1 solutions:
Function: pow
>>> path [int] -> [int]
Found 5 solutions:
increment
decrement
pow
add
decrement -> increment
```

In this example, we show the usage of `define type impls Trait`, where you can specify that a particular type implements a trait. The knowledge base contains the function `add`, which takes two instances of `Add` and adds them, and `sum`, which does the same, but with a `List`. `listify` takes any item, and produces a list containing the item. When we specify that `str` impls `Add`, `str` can be used as an argument to `add`, and `listify` allows creating a list which is then summed, as we see below:
```console
user:~/cpsc312-project/prolog$ swipl main.pl
>>> path [str] -> [str]
Found 0 solutions:
>>> define type str: Add
Added type str<[]>: [Add]
>>> path [str] -> [str] --limit=99
Found 7 solutions (showing 7):
add
listify -> sum
listify -> sum
listify -> sum -> add
listify -> sum -> add
add -> listify -> sum
add -> listify -> sum
```
Paths may be duplicated, or appear to be duplicated, which is largely due to the specializations of a generic function, but all paths will be valid

If you misspell a command, the CLI will offer to correct it and run the corrected version:
```console
user:~/cpsc312-project/prolog$ swipl main.pl
>>> pxth [int] -> [int]
Did you mean path? Type y or n: path [int] -> [int]
Found 5 solutions:
increment
decrement
add
increment -> decrement
increment -> add
```

You can store the current knowledge base and then load it from disk for later usage. In this example, we also show the clear command, which will clear the knowledge base, and the usage of `--strategy=dfs`, which will make the path command generate paths using depth-first search.
```console
user:~/cpsc312-project/prolog$ swipl main.pl
>>> store ./funcs.json
>>> clear
Database has been erased.
>>> path [int] -> [int]
Found 0 solutions:
>>> load ./funcs.json
>>> path [int] -> [int] --strategy=dfs
Found 5 solutions:
increment
increment -> decrement
increment -> decrement -> listify -> sum
increment -> decrement -> listify -> sum -> add
increment -> decrement -> add
```

You can also define traits - which are analogous to Haskell's type classes:
```console
user:~/cpsc312-project/prolog$ swipl main.pl
>>> define trait Example: Bounds
Added trait Example: [Bounds]
```

Through the application, you can set up the natural processing dependencies, using `setup`. If these already exist, `setup` will print `Virtual environment already exists` - otherwise, it will install the dependencies to a virtual environment.
```
user:~/cpsc312-project/prolog$ swipl main.pl
>>> setup
Virtual environment already exists.
```

Finally, `os` will print out the current operating system, and `quit` will shut the program down.
```
user:~/cpsc312-project/prolog$ swipl main.pl
>>> os
Unix
>>> quit
user:~/cpsc312-project/prolog$ 
```

## CLI Parameters
Below is a table which describes support for each key/value pair in the CLI, as well as a description of the inputs to each key:

| Key         | Description                        |
| :---------- | :----------                        |
| name        | Search for a particular name       |
| docs        | Search for documentation           |
| name_cmp    | Comparison method when comparing the string specified by `name` to a particular function name |
| doc_cmp     | Comparison method when comparing the string specified by `docs` to specific documentation |
| strategy    | Strategy used when generating sequences of functions |
| limit       | The number of items (functions, paths) to return for a particular search |

`name/docs`: Any string. If you want to use a string containing whitespace, wrap the string with double quotes and use backslashes to escape backslashes and double quotes.

`strategy` options:
- dfs: Search for a path using depth-first search
- bfs: Search for a path using breadth-first search
- bestfs: Search for a path using best-first search.

`*_cmp` options:
| Key         | Description                        |
| :---------- | :----------                        |
| lev         | Levenshtein distance               |
| subseq      | Subsequence matching (all letters in source appear in same order in target)           |
| substr      | Substring matching (all letters in source appear in same order and adjacent in target) |
| eq          | Strings must be equal |
| re          | Regex match (NOT SUPPORTED FOR VERSIONS < 8.0) |
| fsubstr     | Fuzzy substring matching - this uses the maximum Levenshtein distance for any substring in the target |
| sim         | Natural language similarity, computed by a sentence-by-sentence basis, or over the entire target |
| subsim      | Natural language similarity, computed on sliding window of the target text |
