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

Our MVP omits a GUI, but provides a command line UI and a REST API, which makes it easy for users to build their own interfaces, either by example, or by hooking into the REST API. A full implementation of our product pitch would also incorporate powerful NLP models to work with complex user descriptions, and utilize NLP to improve the quality of searches - for example, by automatically detecting synonyms and allowing these to be incorporated into queries.

Prolog is very suitable for this task, as it makes it intuitive to define knowledge bases and implement search by describing constraint satisfication problems. 

By building systems of constraints, we have already begun to learn the best designs for a search algorithm in Prolog. We also anticipate that building functions for scoring paths can be used to train machine learning models to more intelligently explore the space of user defined functions.

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

Our POC focuses on demonstrating our ability to search a knowledge-base of sample functions (such as print()), create a chain or path of functions and apply scoring algorithms to prioritize the most relevant search results.

It allows the user to:
1. Define functions, including their signatures and documentation, via [JSON files](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/e2c2d877bc0a3a8bea93cbefacea482b12714a15/prolog/function.pl#L135), [a REST API](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/e2c2d877bc0a3a8bea93cbefacea482b12714a15/prolog/server.pl#L78), [or command line input](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/e2c2d877bc0a3a8bea93cbefacea482b12714a15/prolog/main.pl#L220). These functions can be specified with generic arguments.
2. Test that individual functions have a [particular set of features](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/prolog/func_constraints.pl), and [sort said functions with a computed score](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/e2c2d877bc0a3a8bea93cbefacea482b12714a15/prolog/search.pl#L58). 
3. Generate a [sequence of functions](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/e2c2d877bc0a3a8bea93cbefacea482b12714a15/prolog/search.pl#L15) which can transform a provided set of inputs into a provided set of outputs, and satisfy a [provided set of path-specific constraints](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/prolog/path_constraints.pl).

For usage details, and a more specific overview of the CLI/Rest API, go to `How to test and run the code: Prolog`. A high level description of the modules in the prolog directory is provided below.

This represents the core functionality our product aims to provide:
1. a user interface where users can easily define new functions and perform searches over the knowledge base, using constraints to find the most appropriate functions
2. A REST API, which allows any IDE or any language to easily provide powerful search functionality over any codebase with minimal effort

We were already confident that Prolog's search features and easy to extend knowledge base would make it very easy to define functions and search them.
Implementing this POC demonstrates that our belief is indeed correct. We have built the project to be very easy to extend, which makes it easy for us to add new constraint functions, but also makes it easy for users themselves to define and compose their own constraints. In addition, since we cleanly separated the user interfaces from the core functionality, it is very easy to modify one or the other - or even create an entirely new interface, as with our REST API. Accordingly, we have two interfaces - a powerful terminal interface which makes use of our constraint functionality to make commands easily discoverable, and a REST API which allows adding, deleting, and searching functions. We believe that our proof of concept goes above and beyond the requirements, and could easily be considered an MVP.

Being able to implement searches with parameterized constraints gives us confidence that we could extend the search functionality with more powerful constraints, which utilize information derived through natural language processing of the input. Our proof of concept does not currently assign a score to the paths that it generates, which means that users would have to manually evaluate the paths themselves. However, we believe that having implemented scoring for functions, and constraints over paths has given us the necessary knowledge to implement scoring functions for paths as well, without too much additional effort. We would like to combine this with depth-first / best-first search to present the best paths to the user.

During development, we found that Prolog's depth-first search by default is not always the best choice, especially when you want to find the best sequence of functions without having to evaluate all paths. The execution of Prolog programs can also be counter-intuitive at first - for example, "N-1" is lazily evaluated, which can be quite surprising when implementing recursive functions, and it is necessary to use frequently use prolog's cut - `!`.

## Files
- [func_constraints.pl](prolog/func_constraints.pl), [path_constraints.pl](prolog/path_constraints.pl): These files contain function and path constraints, as well as functionality for defining and composing said constraints. Constraints can be used both for rejecting candidate functions and paths, and for scoring them, allowing us to order search results.
- [function.pl](prolog/function.pl): This file contains a set of sample function definitions, functionality for serializing/deserializing functions from/to JSON and Haskell-style signatures, and a primitive type system with generic functions and a limited implementation of typeclasses.
- [main.pl](prolog/main.pl): This file provides a REPL, where users can enter commands and view output. It also provides functionality for finding misspelled commands using Levenshtein distance, and allows users to easily list commands and find instructions for how particular commands are used.
- [search.pl](prolog/search.pl): This file provides `func_path`, `func_path_no_cycles`, `find_func`, and `find_funcs`. These functions allow finding individual functions, or generating chains of functions that transform inputs into outputs. All of these functions accept constraints to filter functions and paths.
- [server.pl](prolog/server.pl): This file provides a basic REST API, where users can define, find, and delete functions. Responses are currently served as a formatted line of text.
- [string_op.pl](prolog/string_op.pl): This file provides functions for common string operations, including subsequence detection, substring matching, Levenshtien distance, and joining lists of strings.
- [server_test.pl](prolog/server_test.pl): A small client, written in Prolog, which sends a series of requests to the REST API implemented in [server.pl](prolog/server.pl) and tests for correct responses.
- [test.pl](prolog/test.pl): Tests for core functionality related to searching, defining functions, and constraints.

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

In the `prolog` directory, you can run `make test` to run the unit tests. You can also load the test file into the swipl repl with `make test-repl` and in that repl you can run `run_tests.` to run those tests.

The project uses the [http](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/http.html%27)), [pcre](https://www.swi-prolog.org/pldoc/man?section=pcre), and [dcg](https://www.swi-prolog.org/pldoc/doc/_SWI_/library/dcg/basics.pl) libraries, which appear to be included by default in SWIPL, and did not require any installation steps when running the project locally.

Please note that using `make prolog-eval` / `make test` will run a a small Python script [prolog/server_test.py](prolog/server_test.py) which tests the REST API. This script launches the REST API ([prolog/main.pl](prolog/main.pl)) and a small client which runs some end-to-end tests ([prolog/server_test.pl](prolog/server_test.pl)) in two separate processes. It does not do any testing of its own, and is only used to run the server and client simultaneously.

#### CLI Overview
This program provides a REPL, which can be run using `swipl main.pl`:
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

and `swipl main.pl --help COMMAND` for instructions for a particular command.
```console
user:~/cpsc312-project/prolog$ swipl main.pl --help define
Defines a function from user input.
Example: define fnName :: [arg1, arg2] -> [output1, output2] | doc 
```

An example session with the CLI, which demonstrates the usage of everything except help and launch:

```console
user:~/cpsc312-project/prolog$ swipl main.pl
>>> pxth [int] -> [int]
Did you mean path? Type y or n: path [int] -> [int]
Found 4 solutions:
increment
increment -> decrement
decrement
decrement -> increment
>>> search [str] -> [int] --name=pant --name_cmp=subseq
Found 2 solutions:
Function: parseInt2
Function: parseInt
>>> define pow :: [int, int] -> [int] | Raises x to the power of e
Adding function: pow
>>> search [int] -> [int] --docs=power --doc_cmp=substr
Found 1 solutions:
Function: pow
>>> store ./funcs.json
>>> clear
All functions have been erased.
>>> path [int] -> [int]
Found 0 solutions:
>>> load ./funcs.json
>>> path [int] -> [int]
Found 5 solutions:
increment
increment -> decrement
increment -> decrement -> pow
increment -> pow
increment -> pow -> decrement
>>> os
Unix
>>> quit
```

Both the CLI and REST API support "name_cmp" and "doc_cmp" for searching function names and documentation, respectively, and compares these against the target fields "name" and "docs". Supported comparision keys are "lev" (Levenshtein), "subseq" (subsequence), "substr" (substring), "eq" (exact string match), and "re" (regex match).

#### REST API Overview
Additionally, it is possible to launch the server for the REST API:
```console
user:~/cpsc312-project/prolog$ swipl main.pl launch 5000
% Started server at http://localhost:5000/
>>> 
```

Example usage of this REST API from Prolog can be found at [prolog/server_test.pl](prolog/server_test.pl). To run [prolog/server_test.pl](prolog/server_test.pl), the server must be already be running in a separate processes. `make test` does this by calling [prolog/server_test.py](prolog/server_test.py). 

The server provides the `func` endpoint, which supports `get` (find a function), `post` (add a function), and `delete` (delete a function) requests. Arguments for these endpoints are provided as HTTP parameters. 

Example usage with Python's requests library:

```python
# Finds a function with the letters "pat" in sequential order
>>> requests.get("http://localhost:5000/func", params={"name": "pat", "name_cmp": "subseq"}).content
b'Found func: parseInt2 :: [str] -> [int] | documentation\n'
# Deletes the parseInt2 function
>>> requests.delete("http://localhost:5000/func", params={"name": "parseInt2"}).content
b'Removed func parseInt2\n'
# Check that parseInt2 is gone
>>> requests.get("http://localhost:5000/func", params={"name": "parseInt2", "name_cmp": "eq"}).content
b'No matching func found: parseInt2 :: ? -> ? | none\n'
# Insert new parseInt2
>>> requests.post("http://localhost:5000/func", params={"name": "parseInt2", "inputs": ["str"], "outputs": ["int"], "docs": "too cool for documentation"}).content
b'Created func parseInt2\n'
# parseInt2 is restored
>>> requests.get("http://localhost:5000/func", params={"name": "parseInt2", "name_cmp": "eq"}).content
b'Found func: parseInt2 :: [str] -> [int] | too cool for documentation\n'
```

Due to the behaviour of Prolog's http library, specifying that a function has no arguments/output requires using boolean parameters "no_inputs" and "no_outputs", respectively.
