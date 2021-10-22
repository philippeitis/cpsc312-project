# C is for Coding
 
As we move further into the digital age, knowing how computer programs work is becoming a more important skill for employment opportunities and understanding the world we live in. Parents are looking for and spending money on educational products that help children learn this essential skill. "C is for Coding" is a fun, interactive way for children to take their first step on this journey in an easy to use, natural language environment.
 
This project is in fulfillment of the [CPSC 312 2021W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website/project.html).
 
## Team Members
 
Our team is:
 
+ Philippe Solodov (25117292): SoloDev
+ Sam Ko (98263569): K-O
+ Scott Banducci (80557069): Bandersnatch
 
We call ourselves: Pattern Match This.
 
## Product Pitch
 
Billions of dollars a year are spent on extracurricular educational programs and products by parents who want the best for their children. Competition for quality jobs is increasing and the skills necessary to succeed are becoming more technical and technological. So how can devoted parents help give their kids the best chances in this silicon-based world? Empower them with an early, intuitive understanding of computer programming that sets the foundation for future excellence.
 
"C is for Coding" is a fun, easy-to-use educational aid designed to help young children become familiar with programming. Once a child is able to type at a keyboard, they are ready to start learning on this platform. At its core is natural language processing that removes the barriers of forcing young kids to learn complicated syntax and numerous keywords. Instead, in this playful environment a child can type in their best attempt at a command. Our system will interpret the intent and provide colorful graphical feedback that they can play with. "Draw cat and dog", "Cat likes Dog" might generate images of the animals and implement rules that if separated they will move to sit beside each other.
 
What makes "C is for Coding" unique is not just that it allows for the earliest possible introduction to this world. It also paves the road to a deeper understanding of programming languages. As children become comfortable typing in commands they will construct more complex statements as gentle prompts and hints guide them. "Try using 'if'! :)". Eventually they will gain access to a graphical representation of code similar to what they'll see in high school and expose them to fixed syntax. The computer equivalent of a children's book, "C is for Coding" will change what you thought possible for the ones you love most.
 
 
## Minimal Viable Project
 
Our MVP will deliver 3 core features necessary to build the product above:
 
1. Natural language processing of user input
2. Real-time GUI
3. Functional composition for chaining effects
 
In order to realistically deliver these features we will need to reduce their scope compared to the final product. We will implement only a subset of possible commands, a limited portion of our desired graphical feedback, and support for a few sequential actions.
 
Natural language: We will make use of Haskell's powerful abstract datatypes to interpret natural language and create a simplified pseudo-language that we can then interpret.
 
GUI: The gloss package will enable us to quickly develop a simple, beautiful user facing interface that will display, and potentially animate, images and text on screen.
 
Sequences of effects: Using the flexibility of Haskell's function composition and Typeclasses, we will allow for input interpreted as a combination of commands to correctly execute.
 
We are excited to learn more about natural language programming with a group member who has experience from previous courses. They will be an excellent resource getting us started. We also hope to share in learning more about using Haskell for natural language processing, with companies like Facebook using it for spam filtering. None of us have built a GUI in Haskell and we're interested to see how gloss will allow us to build one. We've all learned about functional composition in CPSC 312 but this project will give us the opportunity to implement this key feature of Haskell in a complex, real world environment.
 
 
## Proof of Concept
 
Our POC focuses on the end-to-end pipeline:
1. take user input [(link)](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/c58e5a72a9dfc24dcafe872cbb9f8d50b2798120/haskell/app/Main.hs#L28)
2. tokenize it using spaCy [(link)](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/c58e5a72a9dfc24dcafe872cbb9f8d50b2798120/haskell/src/Tokenizer.hs#L30)
3. parse the resulting tokens into trees [(link)](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/c58e5a72a9dfc24dcafe872cbb9f8d50b2798120/haskell/src/NLTree.hs#L27)
4. Parse individual elements into program commands with modifiers such as draw orange [(link)](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/haskell/src/Parse.hs)
5. Interpret the commands [(link)](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/master/haskell/src/Interpret.hs)
6. Render the resulting world [(link)](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/c58e5a72a9dfc24dcafe872cbb9f8d50b2798120/haskell/src/World.hs#L36) to screen [(link)](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/c58e5a72a9dfc24dcafe872cbb9f8d50b2798120/haskell/app/Main.hs#L51)
 
This represents the core functionality of our product: allowing a user to type in a natural language command and showing the results on screen. This is the core "game loop" of our product and building it has given us confidence that we can expand scope of commands and refine presentation to create an outstanding MVP.
 
Additional details of our POC:
 
Our proof of concept uses Gloss to accept user input, and show the corresponding output. We draw user input directly into the viewing area. When the user presses enter we draw the result to the screen, which demonstrates that we can provide an interactive feedback loop in Haskell. The ease of which we accomplished this gives us confidence that we may expand the scope of the input - for instance, allowing someone to select a specific entity on screen using their mouse.
 
Being able to tokenize the input with the powerful natural language models provided by `spaCy`, using shell calls written in Haskell, makes us confident that our system will be able to handle a variety of input. Knowing this, and knowing that we can always upgrade to a more powerful English language model provided by `spaCy`, (or even models for other languages) makes us confident that our project will not be limited by the language models included.
 
While we were already confident that Haskell's abstract data types and pattern matching would make parsing the tokens that `spaCy` produces into useful structures for our project straightforward, actually building out the functionality helped reinforce this confidence. While this is currently the weakest part of our project, it is easy to see how we could extend this to support a variety of input, without compromising on expressiveness.
 
### How to test and run the code: Haskell
Our proof of concept provides a GUI where you can input a sentence and hit enter to process the sentence and see what happens. To see this proof of concept in action, use `make haskell-run` from the project root. Note when running the full demo, `spaCy` will be downloaded to a new virtual environment in the project directory, with the necessary language models, which should take less than 30s.

You can try drawing various colors of cats in the GUI by typing "draw a `color` cat".

To run the tests, which test smaller portions of the project, and use pretokenized data (so `spaCy` will not be downloaded), use `make haskell-eval` from the project.

Notes (not required to test/run code):
- Due to OS specific dependencies, we use `stack-unix.yaml` and `stack-win.yaml`, and the top level `Makefile` will copy the correct version to `stack.yaml`. This is required because of https://github.com/commercialhaskell/stack/issues/3369).
- We bundle freeglut.dll / glut32.dll to run gloss on Windows

### Previously written
As it is currently set up, editing works best if you first `cd` into the `haskell` subdirectory and open VS Code on that directory (`code .`). There is a `Makefile` with some helpful aliases, but you can also just use `stack` as normal.

Note: We expect to be able to test your code by running `stack test`. Included among your tests should be some that demonstrate the core functionality of your code. (We will be running `make haskell-eval` from the project root.)

We should be able to further explore your code's functionality by running `stack ghci`, and you should instruct us on some interesting cases to try.

If you include instructions different from these, be **absolutely sure** that they will work well for us in whatever environment we run your code and that they will be as easy to use as the instructions above!

### How to test and run the code: Prolog

Replace this section with instructions to us for how to test and run your code.

Instructions coming soon, but we expect you'll use the [Prolog Unit Testing](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)) library for testing and that we'll be able to run your code with `swipl`.


