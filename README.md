#  <img src="Documentation/images/zag-icon.png" alt="Zag Icon" width="50"/>  Zag-Smalltalk

Smalltalk VM Written in Zig with methods stored as type-annotated ASTs

Methods are re-constituted from the AST for edit windows.

Execution model:
- No Interpreter
- Threaded execution enables stepwise execution for debugging
- Native Continuation-Passing Style execution model for performance
- switching between the models is seamless 

Memory structure and some of execution principles loosely modelled on OpenSmalltalk VM <br>

Eventually it will include a JIT compiler

More information in the [Documentation](Documentation) - not all completely up to date, but captures the intent.

Papers and Talks can be found [here](Documentation/papers)

The Zag runtime is written in Zig and [this is the auto-generated documentation](https://zag-research.github.io/Zag-Smalltalk/api/) (needs to be commented/structured better, but it will help you navigate the codebase).

(Previously called AST-Smalltalk, but since the implementation is in Zig, a rename seemed obvious.)

## The compiler is written in Smalltalk and currently hosted on Pharo
This includes a compiler to convert an application sitting in the image into a set of classes ready to load into Zag, with all the methods expressed as ASTs.

It is *work-in-progress* to export to a Zag image ready to be run with the Zag runtime mentioned above.

### Loading into Pharo (known to load with Pharo 13)
*very much work-in-progress at the moment*
```smalltalk
Metacello new
  baseline: 'ZagSmalltalk';
  repository: 'github://Zag-Research/Zag-Smalltalk:main';
  load
```

## Commit logs
[Commit logs are visible to the team here](https://github.com/Zag-Research/Zag-Smalltalk/actions/workflows/bench.yml)
