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
The project proposal is available at [PROPOSAL.md](https://github.students.cs.ubc.ca/ph1l1pp3/cpsc312-project/blob/a853728ae242497fe15789b5c17b9471d7db3820/PROPOSAL.md). Some information may be duplicated between this file and PROPOSAL.md.

## MVP Guide
- How the MVP fulfills your proposal
### Links
Clear explanation and links into a handful of key elements of the MVP code that successfully illustrate the proposal requirements

### Running MVP
How to run the MVP

## Guide to New Learning
A guide to your new learning (which can also be part of your guide to the MVP if you prefer, but make clear that that’s what you’re doing!), including:
- Highlighting how the new learning is essential for your project/MVP,
- Direct links into parts of the code where the new learning was employed
- Explanation of how the project benefits from the application of your new learning.

### Prolog features
- Metapredicates
  - Makes defining complex, multifactored constraints much simpler, and simplifies process of allowing users to compose their own constraints
- Dictionary
  - Useful for JSON persistence, as well as collections intended for lookup (eg. a set of options)
- Modules
  - makes it easier to manage a complex Prolog project with many features - serialization/deserialization, various types of constraints, a server interface, etc.
- findall/foreach/forall/setof/findnsols functions
  - allows defining simple path/search predicates which simply check that one path/func is valid, and then aggregating as many solutions as needed
- DCG for parsing
  - makes it possible to construct complex function signatures from simple components
- Conditional compilation with if
  - allows enabling/disabling features for specific versions of Prolog to provide minimum functionality (such as in the CS server / home computer)
- Tabling
  - allows making expensive operations (eg. levenshtein) cheaper without polluting global namespace and without manually implementing necessary garbage collection
- Dynamic values
  - allow easy access to database without passing additional parameters with a list of all visible functions/traits/types

## RUBRIC:
https://steven-wolfman.github.io/cpsc-312-website/project.html#final-project-rubric
