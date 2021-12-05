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

We believe that our project addresses an important need in the software development space, which is an easy to use, powerful, and very flexible search engine, for functions and types which can be used for any programming language, using a variety of interfaces. Such a tool can greatly simplify the process of exploring documentation, as you can use both type signatures and phrases to find relevant functions quickly. In addition, our project makes significant use of many of Prolog's advanced features, as we describe in the `Guide to New Learning section`, to provide flexible search functionality, and three distinct interfaces. It is also very polished - each user interface is designed to be discoverable - the web interface is very stylistic and usage is apparent, the REST API includes an OpenAPI schema which would help language implementors integrate our software, and our command line interface can correct commends and provide explanations for how commands are used. We believe our project goes well above and beyond the expectations for the MVP, and in many senses, approaches the spirit of our product pitch.

### Running MVP
Below, we present two options for running the code. To ensure that all dependencies are up to date and that your experience matches ours, use the commands in the `Using Docker` column - these commands use Docker to ensure a consistent and complete experience. The commands in `Using Current OS` are equivalent, but you may find that the REST API and regex capabilities of our code are not functional (this is because they require SWIPL 8.2+, but the department computers currently only have SWIPL 7.6.4).

| Task | Using Docker | Using Current OS |
| :--  | :-- | :-- |
| Run tests | `make docker-test` | `make prolog-eval` or `cd prolog && make test` |
| Enter swipl repl for Prolog backend | `make docker-repl` | `cd prolog && make test-repl` |
| Launch server listening on `PORT` (default value is 4999) | `make FASTFUNC_SERVER_PORT=PORT docker-server` | `cd prolog && make FASTFUNC_SERVER_PORT=PORT launch-server` |

Launching the server will output the following text:
```console
...
Started server at http://localhost:4999/
Go to http://localhost:4999/openapi to interact with the OpenAPI specification
Go to http://localhost:4999/main.html to interact with the application
docker run -it -p 4999:5000 fast-func-server
>>> 
```

By going to http://localhost:4999/main.html, you can interact with the web-based FastFunc UI, which allows searching for functions, adding new functions, and deleting functions as necessary, and provides a significantly more discoverable interface compared to the command line.

By going to http://localhost:4999/openapi, you can interact with the OpenAPI specification with [RapiDoc](https://mrin9.github.io/RapiDoc/), which provides schemas for all of the endpoints the server provides and allows user interaction. This should look like [this image](./resources/openapi.png).

#### Prolog Dependencies
The project uses the [http](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/http.html%27)), [pcre](https://www.swi-prolog.org/pldoc/man?section=pcre), and [dcg](https://www.swi-prolog.org/pldoc/doc/_SWI_/library/dcg/basics.pl) libraries. If it is run with SWIPL 8.+, all functionality will be available. If not, then the `pcre` library will not be imported, and only the JSON capabilities of the `http` library will be used. This has been tested on the department computers using SWIPL 7.6.4, and using SWIPL 8.4 locally, and all tests pass.

#### Python Dependencies
This project uses Python 3.6+ and [spaCy](https://spacy.io/), a natural language processing library. spaCy is automatically installed into a virtual environment if necessary by all build targets. This will also download a ~50MB natural language model. 

[USAGE.md](USAGE.md) goes into further detail about how to use FastFunc, and provides specific examples of different tasks to help direct your evaluation.

### Go into more details about how the MVP works...

## Guide to New Learning
A guide to your new learning (which can also be part of your guide to the MVP if you prefer, but make clear that that’s what you’re doing!), including:
- Highlighting how the new learning is essential for your project/MVP,
- Direct links into parts of the code where the new learning was employed
- Explanation of how the project benefits from the application of your new learning.

### Prolog features
- Partial function application [LINK](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/8dfb86de7d795e5b02e430ef196415529c8df8fb/prolog/search.pl#L160)
  - This makes it easy to pass relevant parameters to constraint functions, while still providing a consistent API between all constraints
  - There is no need to use bespoke structures to store the function and parameters, which greatly simplifies the API
- Metapredicates [LINK](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/8dfb86de7d795e5b02e430ef196415529c8df8fb/prolog/search.pl#L14)
  - Makes defining complex, multifactored constraints much simpler, and simplifies process of allowing users to compose their own constraints
  - We can compose a complex constraint combining five other constraints, negating some constraints and 
- Dictionaries [LINK](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/8dfb86de7d795e5b02e430ef196415529c8df8fb/prolog/main.pl#L32) [LINK](/prolog/function/serde.pl)
  - Useful for JSON representation, which allows persistence and also serving / reading from a REST API - this is what is used in our web interface!
  - Can be used for collections intended for lookup (eg. a set of options)
- Modules (all pl files except test files and main are modules)
  - This makes it easier to manage a complex Prolog project with many features, such as:
  - serialization/deserialization
  - various types of constraints
  - a server/web interface
- findall/foreach/forall/setof/findnsols functions [LINK](/prolog/search.pl), and throughout the source code
  - These allow defining simple path and search predicates which simply check that one path or item is valid, and then we can aggregate as [many solutions as needed](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/8dfb86de7d795e5b02e430ef196415529c8df8fb/prolog/main.pl#L183)
- DCG for parsing [LINK](/prolog/function/parse.pl)
  - DCGs make it possible to define parsers for each of the individual elements of a type declaration or function signature, and then easily combine these parsers to parse a complex and interesting function or type
- Conditional compilation with if [LINK](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/8dfb86de7d795e5b02e430ef196415529c8df8fb/prolog/string_constraints.pl#L69)
  - This allows enabling/disabling features for specific versions of Prolog to provide minimum functionality
  - Eg. The CS servers are using an outdated version of Prolog, which does not include the PCRE library and HTTP server, but our computers do. Using conditional compilation allows everything else to work.
- Tabling [LINK](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/8dfb86de7d795e5b02e430ef196415529c8df8fb/prolog/sequence_ops.pl#L12)
  - allows making expensive operations (eg. computing Levenshtein distance) cheaper without polluting global namespace and without manually implementing necessary garbage collection
  - Eg. 0.3s to compute Levenshtein distance the first time for 100 pairs of strings 50 characters long, but 0s for the second time.
- Dynamic values [LINK](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/8dfb86de7d795e5b02e430ef196415529c8df8fb/prolog/function.pl#L25)
  - allow easy access to relevant functions when performing search, without passing additional parameters with a list of all visible functions/traits/types
  - allows attaching attributes to objects without modifying said objects.
- Custom Operators (sorta) [LINK](/prolog/function_op.pl)
  - One issue that we haave with Prolog is that there are no data types with compile-time checks to ensure the correctness / positions of field names. This makes it inconvenient to refactor definitions using Prolog's term infrastructures, such as our dynamic function definition, to add new fields, and without thorough testing (which we fortunately have), it would be very easy to introduce new bugs
  - Custom operators make it easy to define almost field accessors, and makes refactoring very easy - we could easily add new fields with this approach, or reorder then, or even remove them - this would could be a very simple CTRL+F, with no pattern matching
  - Unfortunately, this feature is not supported before Prolog 8.3, and our approach also makes use of unsupported elements of SWIPL, which means that we do not actively use this functionality (as nice as it would be)

## RUBRIC:
https://steven-wolfman.github.io/cpsc-312-website/project.html#final-project-rubric
