# CLI Overview
This program provides a REPL, which can be initialized by running `swipl main.pl`:
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
```

and `swipl main.pl --help COMMAND` for instructions for a particular command:
```console
user:~/cpsc312-project/prolog$ swipl main.pl --help define
Defines a function from user input.
Example: define fnName :: [arg1, arg2] -> [output1, output2] | doc 
```

## CLI Examples

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
add
pow
decrement -> increment
```

In this example, we show the usage of `define type impls Trait`, where you can specify that a particular type implements a trait. The knowledge base contains the function `add`, which takes two instances of `Add` and adds them, and `sum`, which does the same, but with a `List`. `listify` takes any item, and produces a list containing the item. When we specify that `str` impls `Add`, `str` can be used as an argument to `add`, and `listify` allows creating a list which is then summed, as we see below:
```console
user:~/cpsc312-project/prolog$ swipl main.pl
>>> path [str] -> [str]
Found 0 solutions:
>>> define str impls Add
Adding impls for str: [Add]
>>> path [str] -> [str] --strategy=dfs
Found 4 solutions:
listify -> sum
listify -> sum -> add
add
add -> listify -> sum
```

## More CLI Examples
If you misspell a command, the CLI will offer to correct it and run the corrected version:
```console
user:~/cpsc312-project/prolog$ swipl main.pl
>>> pxth [int] -> [int]
Did you mean path? Type y or n: path [int] -> [int]
Found 5 solutions:
increment
decrement
add
decrement -> increment
add -> increment
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

In this final example, we demonstrate a few other features. In particular, `os` will print out the current operating system, and `quit` will shut the program down. `define trait ...` provides a mechanism for defining type classes, but this feature is not complete:
```console
user:~/cpsc312-project/prolog$ swipl main.pl
>>> define trait Example: Bounds
User tried to define trait: trait(Example,[Bounds])
>>> os
Unix
>>> quit
```

# REST API Overview

NOTE: The REST API does not work on the department computers, as the http library does not include an HTTP server in SWIPL 7.6.4. This should be available in SWIPL 8.2.4 (http server was added to SWIPL on Nov 13 2020, and SWIPL 8.2.4 was released in Jan 2021).

If testing on a supported version of SWIPL, it is possible to launch the server for the REST API via the CLI:
```console
user:~/cpsc312-project/prolog$ swipl main.pl launch 5000
Started server at http://localhost:5000/
...
>>> 
```

You can test the REST API by running `swipl -g run_tests -t halt server_test.pl` (these tests are only run if SWIPL 8+ is detected). This will launch unique instances of the server for each test, and perform a series of http queries. Each test will automatically select free ports on the machine.

The table below describes all endpoints and supported methods.

| Endpoint/Method         | Description                        | Parameters | Errors | Output |
| :---------- | :----------                        | :--: | :--: | :--- |
| [func/get](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/306bd38c97d7d48b39148f8e66d21d4aa7c68e11/prolog/server.pl#L155) | Get one or more functions with the described features | name, name_cmp, docs, doc_cmp, inputs, outputs (all optional) | 404 if nothing found | All functions in JSON format |
| [func/post](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/306bd38c97d7d48b39148f8e66d21d4aa7c68e11/prolog/server.pl#L159) | Add the described function | JSON: uuid (optional), name, generics, inputs, outputs, docs (docs optional) | N/A | Uuid in JSON format |
| [func/delete](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/306bd38c97d7d48b39148f8e66d21d4aa7c68e11/prolog/server.pl#L166) | Delete one function with the given uuid | uuid | 404 if uuid not found, 405 if uuid belongs to specialized function | Uuid in JSON format |
| [type/get](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/306bd38c97d7d48b39148f8e66d21d4aa7c68e11/prolog/server.pl#L201) | Get a type with the specific name | name | 404 if nothing found | JSON: name, generics, impls |
| [type/post](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/306bd38c97d7d48b39148f8e66d21d4aa7c68e11/prolog/server.pl#L208) | Create the described type | Type in JSON format | N/A | Message with type name in JSON format |
| [type/delete](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/306bd38c97d7d48b39148f8e66d21d4aa7c68e11/prolog/server.pl#L220) | Delete one type with the given name | name | 404 if type not found | Name in JSON format |


Due to the behaviour of Prolog's http library, specifying that a function has no arguments/output requires using boolean parameters "no_inputs" and "no_outputs", respectively.

## CLI, REST API Parameters
Below is a table which describes support for each key/value pair in the CLI and REST API, as well as a description of the inputs to each key:

| Key         | Description                        | CLI Support | REST API Support |
| :---------- | :----------                        | :--: | :--: |
| name        | Search for a particular name       | ✅ | ✅ |
| docs        | Search for documentation           | ✅ | ✅ |
| name_cmp    | Comparison method when comparing the string specified by `name` to a particular function name | ✅ | ✅ |
| doc_cmp     | Comparison method when comparing the string specified by `docs` to specific documentation | ✅ | ✅ |
| strategy    | Strategy used when generating sequences of functions | ✅ | ❌ |
| no_inputs   | Used in the REST API to specify lack of inputs. In CLI, use `[]`. | ❌ | ✅ |
| no_outputs  | Used in the REST API to specify lack of outputs. In CLI, use `[]`. | ❌ | ✅ |

`name/docs`: Any string. If you want to use a string containing whitespace, wrap the string with double quotes and use backslashes to escape backslashes and double quotes.

`strategy` options:
- dfs: Search for a path using depth-first search
- bfs: Search for a path using breadth-first search
- bestfs: Search for a path using best-first search.

`*_cmp` options:
- lev: Levenshtein
- subseq: Subsequence matching (all letters in source appear in same order in target)
- substr: Substring matching (all letters in source appear in same order and adjacent in target).
- eq: Equality
- re: Regex match (NOT SUPPORTED FOR VERSIONS < 8.0)
