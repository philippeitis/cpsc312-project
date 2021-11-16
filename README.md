# FastFuncs

Software development is consistently on the leading edge of occupational innovation and process improvement. Advancements in machine learning and prediction are helping development environments save developers time and effort by anticipating their intentions and offering suggestions. FastFuncs is a tool that allows users to quickly find functions and automate function composition.

This project is in fulfillment of the [CPSC 312 2021W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html).

## Team Members

Our team is:

+ Philippe Solodov (25117292): SoloDev
+ Sam Ko (98263569): K-O
+ Scott Banducci (80557069): Bandersnatch
+ Luis Victoria (78827979): Tor

We call ourselves: Pattern Match This.

## Product Pitch

The software development industry continues to grow and with it so does the demand for programmers. Many companies require their software developers to work harder and longer in order to increase productivity. This mismanagement can lead to overbearing crunch times, lowering workers' quality of life. Fortunately this industry is populated by adapters, innovators and outside the box thinkers who are ready to learn new technologies to make their workflows easier and more efficient. Modern IDEs already provide simple auto-complete as users type, and services like GitHub's Copilot try to help programmers quickly complete their programs by predicting their intent. FastFuncs is an evolution of these technologies, which gives programmers highly customizable, feature-rich code discovery and autocompletion tools, which avoid many of the pitfalls of existing tools.

FastFuncs is a next generation programming assistant that helps users quickly find and compose functions to perform particular tasks, using state-of-the-art natural language processing techniques to interpret intent and search databases of functions, returning only the most relevant results. Users can quickly import their codebases into FastFuncs, with a variety of easy-to-use interfaces which can be adapted to any user's particular needs. Once this is done, they can:
1. Search their entire codebase with FastFuncs' own predicates, and with their own predicates, with minimal effort
2. Use a natural language description to automatically generate a sequence of functions to accomplish a particular task
3. Provide code-completion which can take into account the surrounding context, including functions and documentation

FastFuncs not only makes it easy to quickly design novel functions, but also makes it possible to improve programs by finding better ways to transform inputs into outputs. Programmers can use this functionality to simplify their programs, or make them more efficient, reducing maintenance burden and costly software bugs.

Investing in your employees' tools with the addition of FastFuncs will allow your employees to maximize their efficiency and, most importantly, their quality of life.


## Minimal Viable Project

Our MVP will deliver 3 core features necessary to build the product above:

1. Consolidation of all functions available given imported and standard libraries into a single knowledge-base.
2. Processing of natural language descriptions to identify suitable functions to satisfy said description
3. A scoring feature which will sort the possible approaches by what is most likely the best option

Our MVP omits a GUI, but provides a command line UI and a REST API, which makes it easy for users to build their own interfaces for their language or IDE, and they could either extend the CLI with a language specific module, or use the REST API to provide the required features. A full implementation of our product pitch would also incorporate powerful NLP models to work with complex user descriptions, and utilize NLP to improve the quality of searches - for example, by automatically detecting synonyms and allowing these to be incorporated into queries. Our MVP does not incorporate these, but will make it easy to add these through a simple constraint interface, which can easily be extended to allow this functionality. 

Prolog is very suitable for representing a codebases' API, and performing searches over it, as it is possible to use simple predicates to compose a complete description of a function or path you want to discover - for example, the user could compose an input constraint, an output constraint, and a regex constraint on the function name to find a function which takes a particular input, produces a particular output, and matches the given regex. Our constraint API also makes it possible to assign scores to particular constraints being satisfied, which makes it possible for users to prioritize a particular feature when performing searches. 

By building systems of constraints, we have already begun to learn the necessary techniques to effectively use Prolog for implementing search algorithms, and have learned about features such as cut, and where they can be used to avoid repeating or producing unexpected results. We have also learned more about Prolog's module system in order to effectively organize all of the individual components for easy discovery. Additionally, we can make use of Prolog's definite clause grammars to make it possible for users to specify their functions in whichever programming style they prefer - the default style we will use is reminiscent of Haskell - a few examples follow:

print is a function which takes an `int`, and produces nothing:

`print :: [int] -> []`

`increment` is a function which takes an `int`, and produces another `int`:

`increment :: [int] -> [int]`

`add` is a generic function which takes two instances of Add, and produces another instance of Add:

`add<X: Add> :: [x, x] -> [x]` 

Documentation can optionally be appended, by using ` | Documentation` at the end of a function signature:

`add<X: Add> :: [x, x] -> [x] | Adds two items.` 

We also anticipate that building functions for scoring paths can be used to train machine learning models to more intelligently explore the space of user defined functions, which would help in the development of the full product.

<!-- Replace this with a description of the minimal viable project you will actually build for CPSC 312 (if this becomes your final project).
It may be as short as a few paragraphs, or it may be longer. It should **definitely** take less than 4 minutes
to read carefully and thoroughly.
 
Make clear:
+ how this builds meaningfully toward your product pitch above, without being nearly as much work,
+ how it builds on the strength and power of the language, and
+ how it leads naturally to learning and applying some new element of the language (including what that element is!)
 
Good goals to aim for are from the top two rubric items for proposal grading:
 
> The minimal viable project (MVP) builds on the strengths and power of the language in exciting ways that will clearly lead to excellent learning for students.
 
Or:
 
> The MVP clearly builds significantly on the language and will lead in interesting and natural ways to learning for the students. -->

## Proof of Concept

Our POC demonstrates our ability to define and search a knowledge-base of sample functions (such as print()), create a chain or path of functions and use scoring algorithms in conjunction with search algorithms to prioritize the most relevant search results. It also demonstrates the usage of DCGs to parse user input and subsequently define functions and options via the CLI.

It allows the user to:
1. Define functions (with documentation), types, and traits (eg. Haskell typeclasses), via [JSON files](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/prolog/function/serde.pl), [a REST API](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/ff7f943340c2071b654beeef6e9d31a66e816b4e/prolog/server.pl#L118), [or command line input](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/ff7f943340c2071b654beeef6e9d31a66e816b4e/prolog/main.pl#L250). Functions and types support generic arguments, allowing for the definition of complex type systems.
2. Test that individual functions have a [particular set of features](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/prolog/func_constraints.pl), and [sort said functions with a computed score](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/ff7f943340c2071b654beeef6e9d31a66e816b4e/prolog/search.pl#L21). 
3. Generate a [sequence of functions](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/ff7f943340c2071b654beeef6e9d31a66e816b4e/prolog/search.pl#L116) which can transform a provided set of inputs into a provided set of outputs, and satisfy a [provided set of path-specific constraints](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/prolog/path_constraints.pl).

For usage details, and a more specific overview of the CLI/Rest API, go to `How to test and run the code: Prolog`. A high level description of the modules in the prolog directory is provided below.

This represents the core functionality our product aims to provide:
1. a user interface where users can easily define new functions, types, and traits, and perform searches over the knowledge base, using constraints to find the most appropriate functions
2. A REST API, which allows any IDE or any language to easily provide powerful search functionality over any codebase with minimal effort

We were already confident that Prolog's search features and easy to extend knowledge base would make it very easy to define functions and search them.
Implementing this POC demonstrates that our belief is indeed correct. The core of our POC is built on a very flexible search system, which makes use of composable function and path constraints which we (and potential users) can easily define and extend. This search system provides a variety of search methods - depth-first, breadth-first, and best-first, which allows choosing between time/space availability and quality of results. On top of this core, we have implemented two interfaces - a powerful terminal interface CLI which is designed to be user-facing, with easily discoverable interactions, and a REST API, which allows IDEs or programming languages to add, define, and search for functions. We believe that our proof of concept goes above and beyond the requirements, and could be considered an MVP.

Since our searches already make use of parameterized constraints, we are confident that we could incorportable constraints built on top of natural language processing, using tokenization or sentence similarity techniques to direct searches. We do not currently assign weights to any of the constraints used during searches, so best-first search will weigh them equally, but given our experience building "generic" functions in Prolog, we believe this should not be overly complex to implement.

We have also used definite clause grammars to parse simple function signatures - so far, this has not been overly difficult, and we believe that we could add additional DCGs for syntax from languages such as `Python` or `Java`, which would make it easy for IDEs or end-users to use this as their code search engine.

During development, we found that the execution of Prolog programs can be counter-intuitive at first - for example, "N-1" is lazily evaluated, which can be quite surprising when implementing recursive functions, and it is necessary to use somewhat frequently use prolog's cut - `!`.

## Files
- [func_constraints.pl](prolog/func_constraints.pl), [path_constraints.pl](prolog/path_constraints.pl): These files contain function and path constraints, as well as functionality for defining and composing said constraints. Constraints can be used both for rejecting candidate functions and paths, and for scoring them, allowing us to order search results.
- [function.pl](prolog/function.pl): This file contains a set of sample function definitions, and a primitive type system with generic functions and a limited implementation of typeclasses.
- [function/serde.pl](prolog/function/serde.pl): functionality for serializing/deserializing functions from/to JSON.
- [function/parse.pl](prolog/function/parse.pl): functionality for parsing Haskell-style function signatures, trait declarations (eg. typeclasses) and types with optional generics.
- [main.pl](prolog/main.pl): This file provides a REPL, where users can enter commands and view output. It also provides functionality for finding misspelled commands using Levenshtein distance, and allows users to easily list commands and find instructions for how particular commands are used.
- [search.pl](prolog/search.pl): This file provides `func_path`, `func_path_no_cycles`, `find_func`, and `find_funcs`. These functions allow finding individual functions, or generating chains of functions that transform inputs into outputs. All of these functions accept constraints to filter functions and paths.
- [server.pl](prolog/server.pl): This file provides a basic REST API, where users can define, find, and delete functions. Responses are currently served as a formatted line of text.
- [sequence_ops.pl](prolog/sequence_ops.pl): This file provides functions for common sequence operations, including subsequence detection, substring matching, Levenshtien distance, joining lists of strings, and checking for subsets.
- [server_test.pl](prolog/server_test.pl): A small client, written in Prolog, which sends a series of requests to the REST API implemented in [server.pl](prolog/server.pl) and tests for correct responses.
- [test.pl](prolog/test.pl): Tests for core functionality related to generating paths, defining functions, serializing and deserializing them to/from JSON, and constraints.

<!-- Replace this with a description of your proof-of-concept. This may be as short as a few paragraphs, or it may be longer.
It should **definitely** take less than 4 minutes to read carefully and thoroughly, though working through and running the
code may take an extra 4 minutes. (Your guidance and links should make it easy for us to work through the code.)
 
Tell us:
 
+ what key element of your project the proof-of-concept focuses on
+ what makes that such an important element
+ how completing this gives you confidence that, with sufficient work, you could complete the full (minimal viable) project
 
Include links (likely even line-level links, which are easy to create in Github) throughout to critical pieces of
the code to make it easy for us to understand what you've accomplished and how it fulfills the requirements.
 
Also include instructions for us to test and run your code. (See our guidelines below.)
 
A good goal to aim for is the top rubric item from proposal grading:
 
> Fully functional proof-of-concept is easy to use and review, and it clearly demonstrates a key element necessary for the overall project. -->
### How to test and run the code: Haskell

Replace this section with instructions to us for how to test and run your code.

As it is currently set up, editing works best if you first `cd` into the `haskell` subdirectory and open VS Code on that directory (`code .`). There is a `Makefile` with some helpful aliases, but you can also just use `stack` as normal.

Note: We expect to be able to test your code by running `stack test`. Included among your tests should be some that demonstrate the core functionality of your code. (We will be running `make haskell-eval` from the project root.)

We should be able to further explore your code's functionality by running `stack ghci`, and you should instruct us on some interesting cases to try.

If you include instructions different from these, be **absolutely sure** that they will work well for us in whatever environment we run your code and that they will be as easy to use as the instructions above!

### How to test and run the code: Prolog

In this section, we cover testing, usage of the CLI define functions, types, and traits, and how to perform various queries over them, plus additional features in the system, and usage of the REST API to do the same.

In the `prolog` directory, you can run `make test` to run the unit tests. You can also load the test file into the swipl repl with `make test-repl` and in that repl you can run `run_tests.` to run those tests.

The project uses the [http](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/http.html%27)), [pcre](https://www.swi-prolog.org/pldoc/man?section=pcre), and [dcg](https://www.swi-prolog.org/pldoc/doc/_SWI_/library/dcg/basics.pl) libraries, which appear to be included by default in SWIPL, and did not require any installation steps when running the project locally.

Please note that using `make prolog-eval` or `make test` will run a a small Python script [prolog/server_test.py](prolog/server_test.py) which tests the REST API by coordinating the Prolog server and a Prolog client on the same port.

Example usage of [prolog/server_test.py](prolog/server_test.py):

```console
user:~/cpsc312-project/prolog$ python3 server_test.py 9999
% PL-Unit: end-to-end test . done
% test passed
```
If, by chance, the default port used to launch the servers collides with ports being used on the computer, do `make FASTFUNC_SERVER_PORT=PORT makefile` with either `prolog-eval` or `test`, and setting `PORT` to a currently unused port.

If you encounter issues with running the Makefile due to the script, a short description of the script is provided below:

The script takes `PORT` as an argument. It then calls `swipl main.pl launch PORT` ([prolog/main.pl](prolog/main.pl)) to launch the REST API on the specified port, and launches a small client which runs some end-to-end tests ([prolog/server_test.pl](prolog/server_test.pl)). The script does not do any testing of its own, and is only used to run the server and client simultaneously in two separate processes. 

#### CLI Overview
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

We go over a number of examples of using the FastFunc CLI interface in the following section, where we provide an explanation of an action, followed by an example of how this action is performed.

#### CLI Examples
The primary path composition and search functionality has settings which can be set using `--KEY=VALUE` style arguments, and accepts the same syntax for defining function signatures as described in the MVP, though function names and documentation are omitted, as these are optional. An example of the `path` and `search` commands follows:

If you want to find at most 3 function paths, which accept an `int`, and produce an `int`:
```console
user:~/cpsc312-project/prolog$ swipl main.pl
>>> path [int] -> [int] --limit=3
Found 3 solutions:
increment
decrement
add
```

If you want to find a function whose name matches the regex `p.*a.*n.*t.*[0-9]`:
```console
user:~/cpsc312-project/prolog$ swipl main.pl
>>> search [str] -> [int] --name=p.*a.*n.*t.*[0-9] --name_cmp=re
Found 1 solutions:
Function: parseInt2
```

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

In this example, we show the usage of `define type impls Trait`, where you can specify that a particular type implements a trait. The knowledge base contains the function `add`, which takes two instances of `Add` and adds them, and `sum`, which does the same, but with a `List`. `listify` takes any item, and produces a list containing the item. When we specify that `str` impls `Add`, `str` can be used as an argument to `add`, and `listify` allows creating a list which is then summed, as we see below:
```console
user:~/cpsc312-project/prolog$ swipl main.pl
>>> path [str] -> [str]
Found 0 solutions:
>>> define str impls Add
Adding impls for str: [Add]
>>> path [str] -> [str]
Found 4 solutions:
add
listify -> sum
add -> listify -> sum
listify -> sum -> add
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

`CLI, REST API Parameters` provides an overview of the parameters available for the `search` and `path` commands, and is found at the end.

#### REST API Overview
It is also possible to launch the server for the REST API via the CLI:
```console
user:~/cpsc312-project/prolog$ swipl main.pl launch 5000
% Started server at http://localhost:5000/
>>> 
```

After launching the server, you can test the REST API by running `swipl -g run_tests -t halt server_test.pl`. `make test` does this by calling [prolog/server_test.py](prolog/server_test.py), which launches the server and the REST API. 

The server provides the `func` endpoint, which supports `get` (find a function), `post` (add a function), and `delete` (delete a function) requests. Arguments for these endpoints are provided as HTTP parameters, and the avaiable parameters are described in `CLI, REST API Parameters` below. 

Example usage with Python's requests library (some portions omitted for brevity):

```python
# Finds a function with the letters "pat" in sequential order
>>> requests.get("http://localhost:5000/func", params={"name": "pat", "name_cmp": "subseq"}).json()
{'functions': [{'name': 'parseInt', ...}, {'name': 'parseInt2', 'uuid':'a514694a-46c7-11ec-b2a6-07793d83fe3e', ...}], 'msg': 'Found functions'}
# Deletes the parseInt2 function
>>> requests.delete("http://localhost:5000/func", params={"uuid": 'a514694a-46c7-11ec-b2a6-07793d83fe3e'}).json()
{'msg': 'Removed', 'uuid': 'a514694a-46c7-11ec-b2a6-07793d83fe3e'}
# Check that parseInt2 is gone
>>> requests.get("http://localhost:5000/func", params={"name": "parseInt2", "name_cmp": "eq"}).json()
{'msg': 'No matching func found: parseInt2 :: ? -> ? | none'}
# Insert new parseInt2
>>> requests.post("http://localhost:5000/func", params={"name": "parseInt2", "inputs": ["str"], "outputs": ["int"], "docs": "too cool for documentation"}).json()
{'msg': 'Created func parseInt2', 'uuid': '3b09fb56-46b7-11ec-ba2b-b7cea82a1c5b'}
# parseInt2 is restored
>>> requests.get("http://localhost:5000/func", params={"name": "parseInt2", "name_cmp": "eq"}).json()
{'functions': [{'name': 'parseInt2', ...}], 'msg': 'Found functions'}
```

Due to the behaviour of Prolog's http library, specifying that a function has no arguments/output requires using boolean parameters "no_inputs" and "no_outputs", respectively.

#### CLI, REST API Parameters
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
- re: Regex match
