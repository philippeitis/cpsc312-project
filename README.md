<p align="center">
  <img width="256" height="256" alt="FastFunc logo" src="/prolog/web_content/android-chrome-512x512.png">
</p>

# FastFuncs

Software development is consistently on the leading edge of occupational innovation and process improvement. Advancements in machine learning and prediction are helping development environments save developers time and effort by anticipating their intentions and offering suggestions. FastFuncs is a tool that allows users to quickly find functions and automate function composition.

This project is in fulfillment of the [CPSC 312 2021W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html).

## Team Members

Our team is:

+ Philippe Solodov (25117292): SoloDev
+ Sam Ko (98263569): K-O
+ Scott Banducci (80557069): Bandersnatch
+ Luis Victoria (78827979): Tor

We call ourselves: Functionator.

## Video
Your key additions will be to include and document your MVP and to include (a link to) a demo video of maximum length 3 minutes 59 seconds that demonstrates the functionality of your MVP.

A link to your video (described above). We expect most people will host on YouTube, Vimeo, or the like, but if you need help deciding where to host the video, let us know (well in advance of the deadline!).

Your video should include:

- A title for your project
- Your team name (as used for your project submission) and the names of your team members (identified by your preferred names)
- A demonstration sufficient for us to mark the “Fulfills the proposal” rubric item below. At minimum:
  - Remind us what you proposed (which will often involve mentioning pieces of functionality as you demonstrate them).
  - Show us your MVP running and fulfilling that proposal. Use this opportunity to show off what your MVP can do!
  - Briefly highlight key elements of your code underpinning your functionality. (But have links directly to those places in your code in your README.md file as well, since a video may not be the easiest way to review code!)
- If you have time, also include what we need to judge the “Application of new learning” rubric item, but that is not required.
- Optionally: Close with your team’s CPSC 312-related catchphrase visually illustrated in some fantastically cool way. (But that’s seriously optional!)

## Proposal
The project proposal is available at [PROPOSAL.md](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/PROPOSAL.md#minimal-viable-project). Some information may be duplicated between this file and PROPOSAL.md.

## MVP Guide
Our MVP demonstrates our ability to define and search a knowledge-base of sample functions (such as print()), create a chain or path of functions and use scoring algorithms in conjunction with search algorithms to prioritize the most relevant search results. It also demonstrates the usage of DCGs to parse user input and subsequently define functions and options via the CLI. 

The key elements of our MVP are linked below - these demonstrate what our project allows users to do:
1. Easily define functions (with documentation), types, and traits (eg. Haskell typeclasses), via [a web interface](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/prolog/web_content/main.html), [JSON files](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/prolog/function/serde.pl), [a REST API](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/8dfb86de7d795e5b02e430ef196415529c8df8fb/prolog/server.pl#L150), [or command line input](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/8dfb86de7d795e5b02e430ef196415529c8df8fb/prolog/main.pl#L261). Functions and types support generic arguments, allowing for the definition of complex type system.
  - This satisfies our goal of allowing users to import their code bases into our program, as they have access to a simple JSON format which can easily be loaded into the knowledge base. Additionally, users could define their own parsers for their language of choice within FastFunc itself, if so desired.
2. Test that individual functions and types match [specific string patterns](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/prolog/string_constraints.pl), that functions have [particular set of features](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/prolog/func_constraints.pl), [combine these constraints](/prolog/constraints.pl), and then [sort results with a computed score](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/8dfb86de7d795e5b02e430ef196415529c8df8fb/prolog/search.pl#L33), allowing users to select the best option for a particular task.
  - Included in this is [functionality which uses spaCy](/prolog/nlp.pl), a Python NLP library, to compare user provided text against the names and documentation of items in the codebase to find those which are most relevant, satisfying our goal of using natural language processing for searching code bases.
3. Generate a [sequence of functions](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/8dfb86de7d795e5b02e430ef196415529c8df8fb/prolog/search.pl#L130) which can transform a provided set of inputs into a provided set of outputs, and satisfy a [provided set of path-specific constraints](/prolog/path_constraints.pl), with the same scoring functionality as for individual function searches.

This represents the core functionality our product aims to provide:
1. a user interface where users can easily define new functions, types, and traits, and perform searches over the knowledge base, using constraints to find the most appropriate functions
2. A REST API, which allows any IDE or any language to easily provide powerful search functionality over any codebase with minimal effort
3. Sorting search results using interesting scoring algorithms - our composable constraint interface allows users to mix and match constraints as needed, tracking any relevant state, and even adjusting the weights of particular constraints to prioritize certain features in their search.
4. Processing of natural language descriptions - we provide Levenshtein distance, for both complete strings and for fuzzy substring matching, similarity computed via natural language processing, and regular expressions, which gives users many choices for finding the items in the code base that they are looking for.

We believe that our project addresses an important need in the software development space, which is an easy to use, powerful, and very flexible search engine, for functions and types, which can be used for any programming language, using a variety of interfaces. Such a tool can greatly simplify the process of exploring documentation, as you can use both type signatures and phrases to find relevant functions quickly.

In addition, our project makes significant use of many of Prolog's advanced features, as described in the `Guide to New Learning` section, to provide flexible search functionality, and three distinct interfaces. It is also very polished - the code base is thoroughly documentated, and effort has been put into optimizing the software (eg. [Levenshtein distance was implemented using the O(nm) dynamic programming method](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/c8ff49a34bb6d57684651ee2f5ef457f79e8a5ef/prolog/sequence_ops.pl#L87)). Additionally, we have built each user interface to be discoverable - the web interface is very stylistic and usage is apparent, the REST API includes an OpenAPI schema displayed through RapiDoc, which would help language implementors integrate our software, and our command line interface can seamlessly correct typos in user commands and provide explanations for how commands are used. 

Based on all of these factors, we believe our project goes well above and beyond the expectations for the MVP, and in many senses, approaches the goals laid out for our product pitch.

A complete index of our codebase is provided at [File Overview](./README.md#file-overview).

### Running MVP
Below, we present two options for running the code. To ensure that all dependencies are up to date and that your experience matches ours, use the commands in the `Using Docker` column - these commands use Docker to ensure a consistent and complete experience. The commands in `Using Current OS` are equivalent, but you may find that the REST API and regex capabilities of our code are not functional (this is because they require SWIPL 8.2+, but the department computers currently only have SWIPL 7.6.4).

| Task | Using Docker | Using Current OS |
| :--  | :-- | :-- |
| Run tests | `make docker-test` | `make prolog-eval` or `cd prolog && make test` |
| Enter swipl repl for Prolog backend | `make docker-repl` | `cd prolog && make test-repl` |
| Launch server listening on `PORT` (default value is 4999) | `make FASTFUNC_SERVER_PORT=PORT docker-server` | `cd prolog && make FASTFUNC_SERVER_PORT=PORT launch-server` |

Launching the server will output the following text (though it will be slightly different between using the Docker / OS command):
```console
...
Started server at http://localhost:4999/
Go to http://localhost:4999/openapi to interact with the OpenAPI specification
Go to http://localhost:4999/main.html to interact with the application
docker run -it -p 4999:5000 fast-func-server
>>> 
```

By going to http://localhost:4999/main.html, you can interact with the web-based FastFunc UI, which is more discoverable compared to the command line interface, and makes it much easier to see everything in action. This interface provides the following features:
1. Searching for functions using using the `Function Search` form
2. Bookmarking selected functions by selecting them and pressing `Save`
3. Deleting functions, by clicking the trash can
4. Adding new functions by using the `Add Function to Knowledgebase` form and pressing `Confirm`

You will also find that `Path Search` is linked, which will take you to http://localhost:4999/path.html. On this page, you can search for paths using the `Path Search` form (and pressing submit), or navigate back to the main page by pressing `Function Search`. This will display a list of paths, and for each individual path, you can then click on individual function names to navigate to their descriptions.

By going to http://localhost:4999/openapi, you can interact with the OpenAPI specification with [RapiDoc](https://mrin9.github.io/RapiDoc/), which provides schemas for all of the endpoints the server provides and allows user interaction. This would be used by IDE / language developers to quickly explore the API. The page should look like [this image](./resources/openapi.png).

[USAGE.md](USAGE.md) goes into further detail about how to use FastFunc through the command line interface, providing specific examples of different tasks to help direct your evaluation.

Due to our primary UI existing as a web page, we have decided to include video snippets for each of the task that can be performed through our website:

[Adding a function to the database](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/resources/Add%20Function%20string_to_list.mp4)

[Deleting a function from the database](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/resources/Delete%20function%20member.mp4)

[Using string comparison methods to search documentation](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/resources/Function%20Name%20Print%20-%20Levenshtein.mp4)

[Using string comparison methods to search documentation](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/resources/Doc%20Inte%20-Substring.mp4)

[Saving a function for further reference](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/resources/Input%20int%20-%20Save%20Function.mp4)

![Finding a series of functions which take a particular input, and produce some output](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/resources/Path%20Search%20Int%20to%20Bool.mp4)

#### Prolog Dependencies
The project uses the [http](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/http.html%27)), [pcre](https://www.swi-prolog.org/pldoc/man?section=pcre), and [dcg](https://www.swi-prolog.org/pldoc/doc/_SWI_/library/dcg/basics.pl) libraries. If it is run with SWIPL 8.+, all functionality will be available. If not, then the `pcre` library will not be imported, and only the JSON capabilities of the `http` library will be used. This has been tested on the department computers using SWIPL 7.6.4, and using SWIPL 8.4 locally, and all tests pass.

#### Python Dependencies
This project uses Python 3.6+ and [spaCy](https://spacy.io/), a natural language processing library. spaCy is automatically installed into a virtual environment if necessary by all build targets. This will also download a ~50MB natural language model. 

#### Website Dependencies
The webpages use [Bootstrap](https://getbootstrap.com/) for CSS styling and SVG icons, and the favicon was generated using [redketchup](https://redketchup.io/favicon-generator).

## Guide to New Learning
A guide to your new learning (which can also be part of your guide to the MVP if you prefer, but make clear that that’s what you’re doing!), including:
- Highlighting how the new learning is essential for your project/MVP,
- Direct links into parts of the code where the new learning was employed
- Explanation of how the project benefits from the application of your new learning.

In this section, we go over the Prolog features that we have learned to use, and provide an overview of all of the files that comprise our application, and the purpose they serve. 

### Prolog features
- [Partial function application](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/980a926a60d622513ae3170f4d1ec298dc2e0204/prolog/search.pl#L177)
  - This makes it easy to parameterize constraint functions, while still providing a consistent API for all constraint functions, which makes it easy to mix and match constraints, and combine them in interesting ways with minimal effort.
  - There is no need to use a specific term to store the function and parameters, which greatly simplifies the API and reduces the possibility of bugs
- [Metapredicates](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/980a926a60d622513ae3170f4d1ec298dc2e0204/prolog/search.pl#L22)
  - This makes defining complex, multifactored constraints much simpler, and simplifies the process of allowing users to compose their own constraints
  - for example, they can require that a particular constraint is satisfied five times, or that two or more constraints must all succeed, or modify the cost of a particular constraint to make it more important in best-first search
- Dictionaries [for option parsing](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/980a926a60d622513ae3170f4d1ec298dc2e0204/prolog/main.pl#L32), [JSON serialization/deserialization](/prolog/function/serde.pl)
  - These are very useful for creating JSON representations of data - this makes it very simply to read/write specific predicates from a file (persistence), and at the same time, can be used to serve data over a REST API - this is what is used in our web interface!
  - Can be used for collections of options, which makes it easy to specify parameters through the command line, and add new ones as needed
- Modules (all pl files except test files and main are modules)
  - This makes it easier to manage a complex Prolog project with many features, since you can export only the parts of a module which should be shared, or even restrict yourself to only importing one or two functions to make it abundantly clear where a particular predicate comes from.
  - Features which were easier to add thanks to this system:
  - serialization/deserialization
  - various types of constraints
  - a server/web interface
- [findall/foreach/forall/setof/findnsols](/prolog/search.pl)
  - These allow defining simple path and search predicates which simply check that one path or item is valid, and then we can aggregate as [many solutions as needed](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/980a926a60d622513ae3170f4d1ec298dc2e0204/prolog/main.pl#L183)
- [DCG for parsing](/prolog/function/parse.pl)
  - DCGs make it easy to define parsers for each of the individual elements of a type declaration or function signature, and then easily combine these parsers to parse a complex and interesting function or type
- [Conditional compilation with if](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/980a926a60d622513ae3170f4d1ec298dc2e0204/prolog/string_constraints.pl#L79)
  - This allows enabling/disabling features for specific versions of Prolog to provide some minimum level of functionality where possible
  - Eg. When we submitted the initial Prolog proposal, the CS servers were using an outdated version of SWIPL, which does not include the PCRE library and HTTP server, but our computers do. Using conditional compilation allows most functionality to be used, and lets us test just the components which are supported
- [Tabling](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/980a926a60d622513ae3170f4d1ec298dc2e0204/prolog/sequence_ops.pl#L11)
  - Tabling makes repeated expensive operations (eg. computing Levenshtein distance) cheaper without polluting the global namespace, and without manually implementing the garbage collection which would be required, which is very helpful
  - Eg. It takes 0.3s to compute Levenshtein distance the first time for 100 pairs of strings 50 characters long, but the second time, it takes 0s
- [Dynamic values](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/8dfb86de7d795e5b02e430ef196415529c8df8fb/prolog/function.pl#L25)
  - This simplifies the process of maintaining and updating a shared knowledge base, and makes passing additional parameters with a list of all visible functions/traits/types optional
  - It also allows attaching attributes to objects without modifying said objects - for example, we can mark a function as specialized in the global database by using it's unique UUID, but attaching this to the function definition would require updating the definitions in multiple places
  - It can further be used to implement singletons - for example, to avoid creating a new instance of our NLP thread each time we call it (which takes several seconds to load, and would cause search requests to take ~100x longer), we store the information in the global database, and access it through a mutex (to avoid race-conditions due to the multithreaded HTTP server)
- [Custom Operators (sorta)](/prolog/function_op.pl)
  - One issue that we have with Prolog is that there are no data types with compile-time checks to ensure the correctness / positions of field names. This makes it inconvenient to refactor definitions using Prolog terms, such as our dynamic function definition, to add new fields, and without thorough testing (which we fortunately have), it would be very easy to introduce new bugs
  - Custom operators make it easy to define approximations to field accessors, and would make refactoring very easy - we could easily add new fields with this approach, or reorder then, or even remove them - this would could be a very simple CTRL+F, with no complicated pattern matching
  - This is also nicer than the alternative of used a function for each field, since these functions will either collide with the global namespace (for example, our functions have uuids, which overlaps with the uuid function), or have prefixes, which doesn't particularly scale (eg. we have traits and types, which one gets the t prefix?) or will be overly verbose, as in SWIPL's record's library
  - Unfortunately, this feature is not supported before SWIPL 8.3, and is not fully supported in SWIPL - for instance, this notation can't be called using `call`, and it also uses unofficial language mechanisms to allow the `X ~ field_name` syntax, which means that we do not actively use this functionality (as nice as it would be - though datatypes in Prolog would be even better)

## File Overview
- [compat.pl](prolog/compat.pl) - This file is used to check if the currently running version of Prolog includes the necessary libraries for specific functionality to be available.
- [constraints.pl](prolog/constraints.pl): This file contains all generic constraints, which can be used for types or functions, and allow combinging constraints, scaling their costs, and even checking that a particular item is, or is not in a particular list. Constraints are used both for rejecting candidate items and paths, and for scoring them, allowing us to order search results by their relevance.
- [string_constraints.pl](prolog/string_constraints.pl): This file contains various string-based constraints, which are all generic over a getter. It also has functionality for adding string constraints from user input - this is used both in the server and in the terminal interface to ensure that both interfaces have a consistent experience.
- [func_constraints.pl](prolog/func_constraints.pl), [path_constraints.pl](prolog/path_constraints.pl): These files contain function and path constraints.
- [function.pl](prolog/function.pl): This file contains a set of sample function definitions, and a primitive type system with generic functions and a limited implementation of typeclasses.
- [function/serde.pl](prolog/function/serde.pl): functionality for serializing/deserializing functions from/to JSON.
- [function/parse.pl](prolog/function/parse.pl): functionality for parsing Haskell-style function signatures, trait declarations (eg. typeclasses) and types with optional generics.
- [main.pl](prolog/main.pl): This file provides a REPL, where users can enter commands and view output. It also provides functionality for finding misspelled commands using Levenshtein distance, and allows users to easily list commands and find instructions for how particular commands are used.
- [nlp.py](prolog/nlp.py), [nlp.pl](prolog/nlp.pl): These two files allow users to take advantage of natural language processing to perform more flexible searches for their functions.
- [search.pl](prolog/search.pl): This file provides `func_path`, `func_path_no_cycles`, `find_item`, and `find_items`. These functions allow finding individual items using the constraint architecture - for example, functions and types, and generating chains of items (in this case, functions) which satisfy a series of constraints. All of these functions accept constraints to filter functions and paths.
- [server.pl](prolog/server.pl): This file provides a basic REST API, where users can define, find, and delete functions and types, and also search for function paths. Responses are currently served in JSON format, and an OpenAPI specification is served through the /openapi endpoint. Please note that this requires a recent version of SWIPL, as the http library in 7.6.4 does not provide an HTTP server. 
- [sequence_ops.pl](prolog/sequence_ops.pl): This file provides functions for common sequence operations, including subsequence detection, substring matching, Levenshtien distance, joining lists of strings, and checking for subsets.
- [storage.pl](prolog/storage.pl): This file provides a storage interface, which can load and store functions from disk - this is currently done in JSON format to be consistent with the server interface.
- [server_test.pl](prolog/server_test.pl): A small client, written in Prolog, which sends a series of requests to the REST API implemented in [server.pl](prolog/server.pl) and tests for correct responses.
- [test.pl](prolog/test.pl): Tests for core functionality related to generating paths, defining functions, serializing and deserializing them to/from JSON, and constraints.
- [web_content/main.html](prolog/web_content/main.html), [web_content/path.html](prolog/web_content/path.html): These two pages serve as the front end for our application, and allow users to find, add, and delete functions, and find paths, respectively.
- [web_content/main.js](prolog/web_content/main.js), [web_content/path.js](prolog/web_content/path.js): These Javascript files power the web-based user interface, and are mostly responsible for sending HTTP requests and rendering the output to screen.

## RUBRIC:
https://steven-wolfman.github.io/cpsc-312-website/project.html#final-project-rubric
