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

We link the key elements of our MVP below, which demonstrate what our project allows users to do:
1. Easily define functions (with documentation), types, and traits (eg. Haskell typeclasses), via [JSON files](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/prolog/function/serde.pl), [a REST API](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/7e1441d1544848be021a02acebcdfbe8326cc422/prolog/server.pl#L154), [or command line input](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/7e1441d1544848be021a02acebcdfbe8326cc422/prolog/main.pl#L261). Functions and types support generic arguments, allowing for the definition of complex type system.
  - This satisfies our goal of allowing users to import their code bases into our program, as they have access to a simple JSON format which can easily be loaded into the knowledge base. Additionally, users could define their own parsers for their language of choice within the program.
2. Test that individual functions have a [particular set of features](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/prolog/func_constraints.pl), and [sort said functions with a computed score](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/7e1441d1544848be021a02acebcdfbe8326cc422/prolog/search.pl#L20), allowing users to select the best option for a particular task.
  - Included in this is functionality which uses spaCy, a Python NLP library, to compare user provided text against the names and documentation of relevant items in the codebase to find the most relevant items, satisfying our goal of using natural language processing for searching code bases.
3. Generate a [sequence of functions](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/7e1441d1544848be021a02acebcdfbe8326cc422/prolog/search.pl#L123) which can transform a provided set of inputs into a provided set of outputs, and satisfy a [provided set of path-specific constraints](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/prolog/path_constraints.pl), with the same scoring functionality as for individual function searches.

This represents the core functionality our product aims to provide:
1. a user interface where users can easily define new functions, types, and traits, and perform searches over the knowledge base, using constraints to find the most appropriate functions
2. A REST API, which allows any IDE or any language to easily provide powerful search functionality over any codebase with minimal effort
3. Sorting search results using interesting scoring algorithms - our composable constraint interface allows users to mix and match constraints as needed, tracking any relevant state, and even adjusting the weights of particular constraints to prioritize certain features in their search.
4. Processing of natural language descriptions - we provide levenshtein distance, for both complete strings and for fuzzy substring matching, similarity computed via natural language processing, and regular expressions, which gives users many choices for finding the items in the code base that they are looking for.

### Running MVP
In this section, we cover testing, and usage of the CLI to define functions and types, and performing various queries over them. Additonal features, including the REST API, are described in the [appendix](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project#appendix).

Below, we present two options for running the code. To ensure that all dependencies are up to date and that your experience matches ours, use the commands in the `Using Docker` column - these commands use Docker to ensure a consistent and complete experience. The commands in `Using Current OS` are equivalent, but you may find that the REST API and regex capabilities of our code are not functional (this is because they require SWIPL 8.2+, but the department computers currently only have SWIPL 7.6.4).

| Task | Using Docker | Using Current OS |
| :--  | :-- | :-- |
| Run tests | `make docker-test` | `make prolog-eval` or `cd prolog && make prolog-eval` |
| Enter swipl repl for Prolog backend | `make docker-repl` | `cd prolog && make test-repl` |
| Launch server listening on `PORT` (default value is 4999) | `make FASTFUNC_SERVER_PORT=PORT docker-server` | `cd prolog && swipl main.pl launch PORT` |

Launching the server will output the following text:
```console
...
Started server at http://localhost:4999/
Go to http://localhost:4999/openapi for a web-based REPL
docker run -it -p 4999:5000 fast-func-server
>>> 
```

By going to the linked site (http://localhost:4999/openapi) you can interact with the REST API by sending requests using a convenient UI, which presents all of the possible input fields, and then performs the request for you and displays the response. This is accomplished by using [Rapidoc](https://mrin9.github.io/RapiDoc/) to display an OpenAPI specification.
This should look like [this image](./resources/openapi.png).

The project uses the [http](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/http.html%27)), [pcre](https://www.swi-prolog.org/pldoc/man?section=pcre), and [dcg](https://www.swi-prolog.org/pldoc/doc/_SWI_/library/dcg/basics.pl) libraries. If it is run with SWIPL 8.+, all functionality will be available. If not, then the `pcre` library will not be imported, and only the JSON capabilities of the `http` library will be used. This has been tested on the department computers using SWIPL 7.6.4, and using SWIPL 8.4 locally, and all tests pass.

[USAGE.md](USAGE.md) goes into further detail about how to use FastFunc, and provides specific examples of different tasks to help direct your evaluation.

### Go into more details about how the MVP works...

## Guide to New Learning
A guide to your new learning (which can also be part of your guide to the MVP if you prefer, but make clear that that’s what you’re doing!), including:
- Highlighting how the new learning is essential for your project/MVP,
- Direct links into parts of the code where the new learning was employed
- Explanation of how the project benefits from the application of your new learning.

### Prolog features
- Partial function application [LINK](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/725935ef0c946d30ebaf7fddaabec8c17e519293/prolog/search.pl#L153)
  - Makes it easy to pass relevant parameters to constraint functions, while still providing a consistent API between all constraints
  - No need to pass a tuple with function and arguments, which is inconvenient.
- Metapredicates [LINK](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/944ccad99886115f329ea9aece7c69ff6d6da58e/prolog/search.pl#L12)
  - Makes defining complex, multifactored constraints much simpler, and simplifies process of allowing users to compose their own constraints
  - We can compose a complex constraint combining five other constraints, negating some constraints and 
- Dictionaries [LINK](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/944ccad99886115f329ea9aece7c69ff6d6da58e/prolog/main.pl#L32) [LINK](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/prolog/function/serde.pl)
  - Useful for JSON representation, which allows persistence and also serving / reading from a REST API
  - Can be used for collections intended for lookup (eg. a set of options)
- Modules (all pl files except test files and main are modules)
  - makes it easier to manage a complex Prolog project with many features
  - serialization/deserialization
  - various types of constraints
  - a server interface, etc.
- findall/foreach/forall/setof/findnsols functions (throughout source code)
  - allows defining simple path/search predicates which simply check that one path/func is valid, and then aggregating as many solutions as needed
- DCG for parsing [LINK](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/prolog/function/parse.pl)
  - DCGs make it possible to define parsers for each of the individual elements of a type declaration or function signature, and then combine these parsers to parse a complex and interesting function or type
- Conditional compilation with if [LINK](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/944ccad99886115f329ea9aece7c69ff6d6da58e/prolog/func_constraints.pl#L70)
  - allows enabling/disabling features for specific versions of Prolog to provide minimum functionality
  - Eg. The CS servers are using an outdated version of Prolog, which does not include the PCRE library and HTTP server, but our computers do. Using conditional compilation allows everything else to work.
- Tabling [LINK](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/944ccad99886115f329ea9aece7c69ff6d6da58e/prolog/sequence_ops.pl#L12)
  - allows making expensive operations (eg. computing Levenshtein distance) cheaper without polluting global namespace and without manually implementing necessary garbage collection
  - Eg. 0.3s to compute Levenshtein distance the first time for 100 pairs of strings 50 characters long, but 0s for the second time.
- Dynamic values [LINK](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/944ccad99886115f329ea9aece7c69ff6d6da58e/prolog/function.pl#L22)
  - allow easy access to relevant functions when performing search, without passing additional parameters with a list of all visible functions/traits/types
  - allows attaching attributes to objects without modifying said objects.
## RUBRIC:
https://steven-wolfman.github.io/cpsc-312-website/project.html#final-project-rubric
