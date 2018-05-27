# extensibly-free-arrows

Proposal: 

Session Area: Concepts 

Abstract: 
In this talk, we'll break open a few critical ideas: extensible data types, free, and arrows. Then, we'll combine all of them to create the Extensibly Free Arrow. Free Arrows allow for extreme flexibilty with effectful programming, and the extensible nature allows for programs that are easier to extend and augment. 

Extensibly - Extensible types are those that can be added to at run time. In Haskell, ADT's are the perfect example of a closed sum type. It is a "closed" type, because you must change the actual definition of the type in the code in order to extend it. Extensible (or open) types are those like type classes, to which new instances can be added at anytime without changing the fundamental definitons. The extensible-sp library allows us to create open sum and product types that naturally encapsulate both effects and errors within a program. 

Free - Free is a functional programming concept that allows for abstraction over effect types. Free programs are simple data structures that defer effectful logic. The programmer can define what a particular effect means later, but use that effect as needed. e.g. a free effect could be printing information, and the programmer doesn't care where printing happens, and can later pick between multiple options for what "print" means, to the screen, to a log file, or across a websocket. 

Arrow - Arrows are generalization of monads, and are a good fit for programs with logical, sequential flows. Arrows provide a useful structure for Free implementations, and a simple syntax for combining operations in sequential flows. For non-sequential actions, Arrow's `proc` syntax allows for the similar flexiblity of a do-block. 

Content Relevancy: 
Examples in this talk will be in Haskell, however the ideas and deisgn patterns are applicable to a large range of functional programming languages. Utilizing this design pattern creates code that is both easy to read and to extend, as well as providing exceptional power for accomplishing complex programming tasks. Attendees will hopefully learn a new, and fairly under utilized portion of functional programming. 