# Technical writing course

Notes and exercise answers from following the [Google Technical Writing Course](https://developers.google.com/tech-writing).

Conventions

- _Italic_ emphasis is used for exercise instructions.

## Tech writing one

## Just enough grammar

#### Nouns

_Identify the six nouns in the following passage:_

> C enables programmers to control pointers an memory. Great power brings great responsibility.

- C
- programmers
- pointers
- memory
- power
- responsibility

#### Pronouns

_Identify the three pronouns in the following passage:_

> The cafeteria featured peashew butter and pluot jam on pumperye toast. Employees found it awesome and wished they could it this every day.

- it
- they
- this

#### Verbs

_Identify the verbs in the following passage:_

> Samantha is coding Operation Bullwinkle in C++. This project currently consumes over 80,000 lines of code. She previously used Python, but recently gravitated to C++. Samantha leads a team of four software engineers, which will grow to six software engineers next quarter.

- is coding
- consumes
- used
- gravitated
- leads
- will grow

#### Adjectives and adverbs

_Identify the four adjectives in the following passage:_

> Engineering is a great career for brilliant minds. I know five engineers who could excel at any intellectual task.

- great
- brilliant
- five
- intellectual

#### Conjunctions and transitions

_Fill in the most appropriate transition:_

> Barbara typically studies problems for a long time before writing the first line of code. \_\_\_\_\_\_\_\_, she spontaneously coded a method the other day when she was suddenly inspired.

- However

### Words

#### Define new or unfamiliar words

Define new or unfamiliar terms but don't reinvent the wheel.

#### Use terms consistently

Use terms consistently but introduce shorthands for convenience.

#### Use acronyms properly

Use acronyms properly, introducing the full term and then the acronym in boldface for emphasis.

- Consider not using acronyms if the full term isn't used more than a few times.
- Consider not using the full term if the acronym has assumed its own meaning; for example, with (HTML).

_Fix the following passage. Assume that this passage is the initial instance of the term MapReduce in the document and that MR is the best abbreviation:_

> Jeff Dean invented MapReduce in 1693, implementing the algorithm on a silicon-based computer fabricated from beach sand, wax-paper, a quill pen, and a toaster oven. His version of MR held several world performance records until 2014.

Jeff Dean invented **MapReduce (MR)** in 1693, implementing the algorithm on a silicon-based computer fabricated from beach sand, wax-paper, a quill pen, and a toaster oven. His version of MR held several world performance records until 2014.

#### Disambiguate pronouns

- Only use a pronoun after you've introduced the noun; never use the pronoun before you've introduced the noun.
- Place pronouns as close as possible to the referring noun. As a rule of thumb, if more than five words separate your noun from your pronoun, consider repeating the noun instead of using the pronoun.
- Consider the ambiguity introduced with each pronoun and use the appropriate noun to disambiguate.

_Identify all possible meanings for the ambiguous pronouns in each of the following passages:_

1. Aparna and Phil share responsibilities with Maysam and Karan and they are the next ones on call.

- Do Aparna and Phil share responsibilities with Maysam and Karan, or just Maysam. In this case; Karan, Aparna, and Phil would all be on the next ones on call.

- It is not clear whether _Aparna and Phil_ or _Maysam and Karan_ are next on call.

2. You may import Carambola data via your configuration file or dynamically at run time. This may be a security risk.

- What is a security risk? Is importing Carambola a security risk itself, or is importing via either a configuration file or dynamically at run time a risk?

### Active voice vs. passive voice

The active voice follows the formula 'actor + verb + target' whereas the passive voice reverses this formula to become 'target + verb + actor'.

_Mark each of the following sentences as either **Passive** or **Active**:_

1. `MutableInput` provides read-only access. **Active**
2. Read-only access is provided by `MutableInput`. **Passive**
3. Performance was measured. **Passive**
4. Python was invented by Guido van Rossum in the twentieth century. **Passive**
5. David Korn discovered the KornShell quite by accident. **Active**
6. This information is used by the policy enforcement team. **Passive**
7. Click the Submit button. **Active**
8. The orbit was calculated by Katharine Johnson. **Passive**

_Each of the following sentences contains two verbs. Categorize each of the verbs in the following sentences as either active or passive. For example, if the first verb is active and the second is passive, write **Active, Passive**._

1. The QA team loves ice cream, but their managers prefer sorbet. **Active, Active**
2. Performance metrics are required by the team, though I prefer wild guesses. **Passive, Active**
3. When software engineers attempt something new and innovative, a reward should be given. **Active, Passive**

_Rewrite the following passive voice sentences as active voice. Only part of certain sentences are in passive voice; ensure that all parts end up as active voice:_

1. The flags were not parsed by the Mungifier. **The Mungifier did not parse the flags.**
2. A wrapper is generated by the Op registration process. **The Op registration process generates a wrapper.**
3. Only one experiment per layer is selected by the Frombus system. **The Frombus system selects only one experiment per layer.**
4. Quality metrics are identified by asterisks; ampersands identify bad metrics. **Asterisks identify quality metrics; ampersands identify bad metrics.**

### Clear sentences

#### Choose strong verbs

Use strong verbs to engage and educate readers and generally avoid forms of the verb _to be_.

_Clarify the following sentences by picking more specific verbs. Along the way, feel free to rearrange the sentences to add, modify, or delete words:_

1. When a variable declaration doesn't have a datatype, a compiler error happens. **The compiler errors when a variable declaration lacks a datatype.**
2. Compiler errors occur when you leave off a semicolon at the end of a statement. **Omitting a semicolon at the end of a statement will trigger a compiler error.**

#### Reduce there is/there are

Avoid generic sentence starters _There is_ or _There are_ and instead create true subjects or verbs for clarity.

_Clarify the following sentences by removing_ There is _, and possibly rearranging, adding, modifying, or deleting other words:_

1. There is a lot of overlap between X and Y. **X and Y overlap a lot.**
2. There is no creator stack for the main thread. **The main thread lacks a creator stack.**
3. There is a low-level, TensorFlow, Python interface to load a saved model. **TensorFlow provides a low-level Python interface to load a saved model.**
4. There is a sharding function named `distribute` that assigns keys. **The `distribute` sharding function assigns keys.**

#### Minimize certain adjectives and adverbs

The purpose of technical writing is to educate and not to market. Use factual data instead of marketing speak.

### Short Sentences

- Shorter documentation reads faster.
- Shorter documentation is typically easier to maintain.
- Extra lines of documentation introduce additional points of failure.

#### Focus each sentence on a single idea

_Convert the following overly long sentence to a series of shorter sentences. Don't revise too much, just end up with a few sentences instead of only one._

> In bash, use the if, then, and fi statements to implement a simple conditional branching block in which the if statement evaluates an expression, the then statement introduces a block of statements to run when the if expression is true, and the fi statement marks the end of the conditional branching block.

In bash, use the if, then, and fi statements to implement a simple conditional branching block. The if statement evaluates an expression. The then statement introduces a block of statements to run when the if expression is true. The fi statement marks the end of the conditional branching block.

#### Convert some long sentences to lists

_Refactor the following sentences into something shorter and clearer. Make sure that your answer contains a list:_

> To get started with the Frambus app, you must first find the app at a suitable store, pay for it using a valid credit or debit card, download it, configure it by assigning a value for the `Foo` variable in the `/etc/Frambus` file, and then run it by saying the magic word twice.
>
> KornShell was invented by David Korn in 1983, then a computer scientist at Bell Labs, as a superset of features, enhancements, and improvements over the Bourne Shell (which it was backwards compatible with), which was invented by Stephen Bourne in 1977 who was also a computer scientist at Bell Labs.

To get started with the Frambus app:

1. Find the app at a suitable store.
2. Pay for the app using a valid credit or debit card.
3. Download the app.
4. Configure the app by assigning a value for the `Foo` variable in the `/etc/Frambus` file.
5. Finally, run the app by saying the magic word twice.

The following two Bell Labs computer scientists invented popular shells:

- David Korn invented the KornShell in 1983.
- Stephen Bourne invented the Bourne Shell in 1977.

The KornShell was a backwards-compatible superset of features, enhancements, and improvements over the Bourne Shell.

#### Eliminate or reduce extraneous words

_Shorten the following sentences without changing their meaning:_

1. In spite of the fact that Arnold writes buggy code, he writes error-free documentation. **Arnold writes buggy code, but error-free documentation.**
2. Changing the sentence from passive voice to active voice enhances the clarification of the key points. **Changing the sentence from passive voice to active voices clarifies the key points.**
3. Determine whether Rikona is able to write code in COBOL. **Determine whether Rikona can code in COBOL.**
4. Frambus causes the production of bugs, which will be chronicled in logs by the LogGenerator method. **Frambus produces bugs which the LogGenerator method logs.**

#### Reduce subordinate clauses

Scrutinize subordinate clauses with the _one sentence = one idea_ formula in mind. If the subordinate clause branches into a separate idea, consider using a separate sentence.

_Determine which of the sentences contain the subordinate clauses that should be branched off into separate sentences. (Don't rewrite the sentences, just identify the sentences that should be rewritten.)_

1. Python is an interpreted language, which means that the language can execute source code directly. **No**
2. Bash is a modern shell scripting language that takes many of its features from KornShell 88, which was developed at Bell Labs. **Yes**
3. Lisp is a programming language that relies on Polish prefix notation, which is one of the systems invented by Polich logician Jan Lukasiewicz. **Yes**
4. I don't want to say that FORTRAN is old, but only radiocarbon dating can determine its true age. **No**

#### Distinguish that from which

- In the United States, **that** is for essential subordinate clauses and **which** is for nonessential subordinate clauses.
- Place a comma before **which**; do not place a comma before **that**.

### Lists and tables

#### Choose the correct type of list

- **bulleted lists**, for _unordered_ items
- **numbered lists**, for _ordered_ items
- **embedded lists** (lists in sentences), generally a poor way to present technical information

_Convert the following paragraph into one or more lists:_

> Today at work, I have to code three unit tests, write a design document, and review Janet's latest document. After work, I have to wash my car without using any water and then dry it without using any towels.

Today at work, I have to:

- Code three unit tests.
- Write a design document.
- Review Janet's latest document.

After work, I have to:

1. Wash my car without using any water.
2. Dry my car without using any towels.

#### Keep list items parallel

All items in a parallel list match along the following parameters:

- grammar
- logical category
- capitalization
- punctuation

_Are the following lists parallel or nonparallel?_

> - Broccoli inspires feelings of love or hate.
> - Potatoes taste delicious.
> - Cabbages.

No. The last list item is not a full sentence; the other list items are full sentences.

> - The red dots represent sick trees.
> - Immature trees are represented by the blue dots.
> - The green dots represent healthy trees.

No. The second list item is passive voice; the other list items are in active voice.

#### Start numbered list items with imperative verbs

Consider starting all items in a numbered list with an imperative verb. An **imperative verb** is a command, such as **open** or **start**.

_Make the following list parallel. Ensure that each element in the result list begins with an imperative verb:_

> 1. Stop Fruvous
> 2. The key configuration file is `/moxy/fruvous`. Open this file with an ASCII text editor.
> 3. In this file, you will see a parameter named Carambola, which is currently set to the default value (32). Change this value to 64.
> 4. When you are finished setting this parameter, save and close the configuration file
> 5. now, start Fruvous again.

1. Stop Fruvous.
2. Open the `/moxy/fruvous` configuration file with an ASCII text editor.
3. Change the `Carambola` parameter from its default value (32) value to 64.
4. Save and close the configuration file.
5. Restart Fruvous.

#### Punctuate items appropriately

If a list item is a sentence, use sentence capitalization and punctuation. Otherwise, do not use sentence capitalization and punctuation.

#### Create useful tables

- Label table columns with meaningful headers.
- Avoid putting too much text into a table cell.
- Strive for parallelism within individual columns.

#### Introduce each list and table

_Write an introductory sentence for the following table:_

| Languages | Inventor          | Year Introduced | Key Feature |
| --------- | ----------------- | --------------- | ----------- |
| Lisp      | John McCarthy     | 1958            | recursion   |
| C++       | Bjarne Stroustrup | 1979            | OOP         |
| Python    | Guido van Rossum  | 1994            | simplicity  |

The following table identifies the inventor, year of invention and key feature of three programming languages.

### Paragraphs

#### Write a great opening sentence

Good opening sentences establish the paragraph's central point.

_Is the opening sentence of the following paragraph effective or defective?_

> The Pythagorean Theorem states that the sum of the squares of both legs of a right triangle is equal to the square of the hypotenuse. The k-means clustering algorithm relies on the Pythagorean Theorem to measure distances. By contrast, the k-median clustering algorithm relies on the Manhattan Distance.

Defective. The description of the Pythagorean Theorem makes more sense after the _k-means clustering algorithm_ sentence. It implies the paragraph will focus on the Pythagorean Theorem and its actual focus is clustering algorithms.

#### Focus each paragraph on a single topic

Restrict each paragraph to the sole current topic.

_Remove the extraneous sentence(s) from the following paragraph. Assume that the opening sentence does establish the desired theme for the paragraph:_

> **Spreadsheets** provide a great way to organize data. Think of a spreadsheet as a table with rows and columns. Spreadsheets also provide mathematical functions, such as means and standard deviations. Each row holds details about one entity. Each column holds details about a particular parameter. For example, you can create a spreadsheet to organize data about different trees. Each row would represent a type of tree. Each column would represent a different characteristic, such as the tree's height or the tree's spread.

**Spreadsheets** provide a great way to organize data. Think of a spreadsheet as a table with rows and columns. ~~Spreadsheets also provide mathematical functions, such as means and standard deviations.~~ Each row holds details about one entity. Each column holds details about a particular parameter. For example, you can create a spreadsheet to organize data about different trees. Each row would represent a type of tree. Each column would represent a different characteristic, such as the tree's height or the tree's spread.

#### Don't make paragraphs too long or too short

Walls of text are visually intimidating. Single sentence paragraphs point to an organizational problem with your writing.

1. **What** are you trying to tell your reader?
2. **Why** is it important for the reader to know this?
3. **How** should the reader use this knowledge? Alternatively, how should the reader know your point to be true?

### Audience

Consider the target audience(s) and their proximity to the presented knowledge.

#### Determine what your audience needs to learn

Write a list of everything you expect your target audience to learn through your documentation.

#### Fit documentation to your audience

- Use appropriate vocabulary and concepts.
- Avoid **the curse of knowledge**, and remember that novices may not know everything you already know.

_Assume that the following paragraph is the start of a paper aimed at physicians who have never programmed before. Identify the aspects of the paragraph that suffer from the curse of knowledge:_

> C is a mid-level language, higher than assembly language but lower than Python or Java. The C language provides programmers fine-grained control over all aspects of a program. For example, using the C Standard Library, it is easy to allocate and free blocks of memory. In C, manipulating pointers directly is mundane.

The previous paragraph assumes the reader understands what is meant by _C_, _Python_, or _Java_. It also assumes a knowledge of programming concepts like working with memory or _manipulating pointers_.

_Suppose the preceding paragraph was aimed at undergraduate computer science students new to C but comfortable with Python. Does the paragraph still suffer from the curse of knowledge?_

The preceding paragraph still suffers from the curse of knowledge. Computer science students experienced with Python may not have experience with _manipulating pointers_.

#### Simple words

English is not the native language of a significant number of technical readers. Therefore, prefer simple words over complex words and avoid using arcane, obsolete, or overly-complex English words.

#### Cultural neutrality and idioms

Keep your writing culturally neutral and avoid idioms.

_Identify the problems with the following sentences:_

> 1. As of Version 3.0, it was still kosher to call the Frambus method.
> 2. Deciding which BorgResourceSpec constraints/preferences are combinable is a sticky wicket.
> 3. Be that as it may, you still have to write unit tests.

1. The word _kosher_ is used which is not culturally neutral.
2. The phrase _sticky wicket_ is a cricket specific term.
3. The idiom _be that as it may_ is used which may not be understood by international audiences or may be mistranslated by translation software.

### Documents

#### State your document's scope

A good document begins by defining its scope. For example:

> This document describes the overall design of Project Frambus.

#### State your audience

A good document explicitly specifies its audience. For example:

> I wrote this document for the test engineers supporting Project Frambus.

#### Establish your key points up front

Ensure that the start of the document answers your readers' essential questions. Use an executive summary on larger documents.

#### Write for your audience

See the _Audience_ section above.

#### Organize the document

Organize the document to supply what readers should know or be able to do after reading the document.

#### Break your topic into sections

Modular documentation is easier to read, understand, maintain, and reuse.
