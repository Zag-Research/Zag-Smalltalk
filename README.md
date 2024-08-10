# Zag-Smalltalk

Smalltalk VM Written in Zig with methods stored as type-annotated ASTs

Methods are re-constituted from the AST for edit windows. 
No Interpreter; uses Threaded and CPS execution models interchangeably 

Memory structure and some of execution principles loosely modelled on OpenSmalltalk VM <br>

Eventually it wiil include a JIT compiler

More information in the [Documentation](Documentation) 

Papers and Talks can be found [here](Documentation/papers)

(Previously called AST-Smalltalk, but since the implementation is in Zig, a rename seemed obvious.)
