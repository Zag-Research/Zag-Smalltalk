# Description of Zag Smalltalk
### Abstract
Zag-Smalltalk is a principle-based Smalltalk VM. "Principled" means that the only 3 operations are: message send, assignment, and return. There is no special-casing of methods like `ifTrue:ifFalse` or `whileTrue:`, although of course some methods are implemented by primitive methods.

The research question is, "Can this be made fast enough to be competitive?"

The basic idea is that methods are maintained in their AST form. An editor might provide a text-based version, and when importing/exporting to files we will generate a text version, but within the image the AST is the canonical form, and the code generator drives directly off the AST^[There is no byte code.].

Another goal is be be as compatible as possible with [Pharo](https://pharo.org) so that we can leverage most of the rich ecosystem!

The system is being developed on https://github.com/Zag-Research/Zag-Smalltalk

## More
- [Execution](Execution.md) is described here, including message dispatch.
- The [memory structure](Mapping.md) and [garbage collector](Memory%20Management.md) are described here.
- [Optimization opportunities](Optimizations.md) are described here.
- [Experiments](Experiments.md)
- [Hacking](Hacking.md) has some guidance on working with the code
- [Papers](papers/README.md) has copies of papers relating to Zag
