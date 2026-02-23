#  <img src="Documentation/images/zag-icon.png" alt="Zag Icon" width="50"/>  Zag-Smalltalk

Smalltalk VM Written in Zig with methods stored as type-annotated ASTs

Methods are re-constituted from the AST for edit windows.

Execution model:
- No Interpreter
- Threaded execution enables stepwise execution for debugging
- Native Continuation-Passing Style execution model for performance
- switching between the models is seamless 

Memory structure and some of execution principles loosely modelled on OpenSmalltalk VM <br>

Eventually it wiil include a JIT compiler

More information in the [Documentation](Documentation) 

Papers and Talks can be found [here](Documentation/papers)

(Previously called AST-Smalltalk, but since the implementation is in Zig, a rename seemed obvious.)

### Loading into Pharo (known to load with Pharo 13)
*very much work-in-progress at the moment*
```smalltalk
Metacello new
  baseline: 'ZagSmalltalk';
  repository: 'github://Zag-Research/Zag-Smalltalk:main';
  load
```
