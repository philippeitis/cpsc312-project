# CPSC 312 Project

Project Template for CPSC 312 Projects. Use this for both your proposal and final project submission.

(Since you are submitting a link to a specific commit, we will be able to tell the difference.)

The template to edit begins below. You can delete this header section once you've begun.

We will post some additional material to include when you reach the final project stage.

# Insert My Excellent Project Title Here

This will be replaced with a very brief (one paragraph, 2-3 sentences) overview of the project.

Leave the following sentence in so you can easily link back to the requirements and *especially* rubric while editing your project:

This project is in fulfillment of the [CPSC 312 2021W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html).

## Team Members

Our team is:

+ Philippe Solodov (25117292): optional awesome nickname 1
+ Sam Ko (98263569): optional awesome nickname 2
+ Scott Banducci (student #?): optional awesome nickname 3
+ If you have a fourth member: Team Member Name 4 (student # 4): optional awesome nickname 4

We call ourselves: optional awesome team name.

## Product Pitch

### Notes
We want to describe the following:
A language for kids to learn the principles of computer science and programming without having to remember syntax or keywords. It is flexible in terms of syntax, since it uses NLP, and is very easy to learn due to its flexible. It has graphical feedback, to make kids have fun while learning
Easy to start:
- Say hello
- Draw a cat
- Put a hat on the cat
Naturally leads to more complex statements, such as 
- Say hello if there is a cat
- Cats are afraid of blue dogs
- ...
Then? Displays structure of program and how simple input can be represented using more traditional program style, with fixed syntax

Makes use of Haskell's powerful abstract datatypes to represent this language and provide a flexible interface, functional composition to make applying sequences of effects very simple.

Functional transformations, pattern matching to make going from NLP intermediate representation to programmatic representation very easy and easily support many variations of syntax

### To Replace
Replace this with a pitch for your project and the problem it solves. This is your vision for what the project
would like like as a complete product, ready for awesome action. (Yes, awesomeness seems to be a theme.)
It may be as short as a couple of paragraphs, or it may be longer. It should **definitely** take less than 4 minutes
to read carefully and thoroughly.

Be sure that this touches clearly on the [project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html#project-requirements).

Good goals to aim for are from the top two rubric items for proposal grading:

> Exciting and language-appropriate product idea tackling a problem that is clearly compelling to a significant audience.

Or:

> Solid and language-appropriate product idea with a problem that is of real interest to the submitting team.

(It's easy to focus on the product and not the problem. Remember to include both!)

## Minimal Viable Project

### NOTES:
Graphical interface running in real time, with a subset of desired functions and supported syntax (eg. Put a cat here ...) and lots of output options such as GUI, videos, or pictures to share.
### TO REPLACE
Replace this with a description of the minimal viable project you will actually build for CPSC 312 (if this becomes your final project).
It may be as short as a few paragraphs, or it may be longer. It should **definitely** take less than 4 minutes
to read carefully and thoroughly.

Make clear:
+ how this builds meaningfully toward your product pitch above, without being nearly as much work,
+ how it builds on the strength and power of the language, and
+ how it leads naturally to learning and applying some new element of the language (including what that element is!)

Good goals to aim for are from the top two rubric items for proposal grading:

> The minimal viable project (MVP) builds on the strengths and power of the language in exciting ways that will clearly lead to excellent learning for students.

Or:

> The MVP clearly builds significantly on the language and will lead in interesting and natural ways to learning for the students.

## Proof of Concept
### NOTES:
- Feedback loop, save image to directory each iteration
- Want to allow simple trees and variations of strings
- Draw some simple pictures and apply simple effects (blue cats, put cat on top)
- Focuses on core functionality, demonstrating that pattern matching is a powerful tool for this problem, and provides a good way to extend functionality
- Key sections:
- tokenization
- tree representation
- transforming tree to graphical structure
- final image output

Our proof of concept demonstrates an end-to-end pipeline, where we:
1. take user input
2. tokenize it using spaCy
3. parse the resulting tokens into trees
4. Interpret the tree in terms of program commands, such as draw
5. Render the resulting tokens to screen

Our proof of concept uses gloss to accept user input, and output the corresponding output. We currently draw user input directly into the viewing area, and once they hit enter, we draw the result of their input to the screen, which demonstrates that we can provide an interactive feedback loop in Haskell. The ease of which we accomplished this gives us confidence that if need be, we can be more thorough with the input we take - for instance, allowing someone to select a specific entity on screen using their mouse.

Being able to tokenize the input with the powerful natural language models provided by `spaCy`, using shell calls written in Haskell, makes us confident that our system will be able to handle a variety of input. Knowing this, and knowing that we can always upgrade to a more powerful model provided by `spaCy`, makes us confident that our project will not be limited by the language models included.

While we were already confident that Haskell's abstract data types and pattern matching would make parsing the tokens that `spaCy` produces into useful structures for our project, actually building out the functionality helped enforce this confidence. While this is currently the weakest part of our project, it is easy to see how we could extend this to support a variety of input, without compromising on expressiveness.


## TO REPLACE
Replace this with a description of your proof-of-concept. This may be as short as a few paragraphs, or it may be longer.
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

> Fully functional proof-of-concept is easy to use and review, and it clearly demonstrates a key element necessary for the overall project.

### How to test and run the code: Haskell

Replace this section with instructions to us for how to test and run your code.

As it is currently set up, editing works best if you first `cd` into the `haskell` subdirectory and open VS Code on that directory (`code .`). There is a `Makefile` with some helpful aliases, but you can also just use `stack` as normal.

Note: We expect to be able to test your code by running `stack test`. Included among your tests should be some that demonstrate the core functionality of your code. (We will be running `make haskell-eval` from the project root.)

We should be able to further explore your code's functionality by running `stack ghci`, and you should instruct us on some interesting cases to try.

If you include instructions different from these, be **absolutely sure** that they will work well for us in whatever environment we run your code and that they will be as easy to use as the instructions above!

### How to test and run the code: Prolog

Replace this section with instructions to us for how to test and run your code.

Instructions coming soon, but we expect you'll use the [Prolog Unit Testing](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)) library for testing and that we'll be able to run your code with `swipl`.


