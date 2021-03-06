# Description of AST Smalltalk
### Abstract
AST-Smalltalk is a principle-based Smalltalk VM. "Principled" means that the only 3 operations are: message send, assignment, and return. There is no special-casing of methods like `ifTrue:ifFalse` or `whileTrue:`, although of course some methods are implemented by primitive methods.

The research question is, "Can this be made fast enough to be competitive?"

The basic idea is that methods are maintained in their AST form. An editor might provide a text-based version, and when importing/exporting to files we will generate a text version, but within the image the AST is the canonical form, and even the interpreter drives off the AST^[There is no byte code.].

Another goal is be be as compatible as possible with [Pharo](https://pharo.org) so that we can leverage most of the rich ecosystem!

## The [AST class structure](AST_Classes.md) is described here.

## The [interpreter](Interpreter.md) is described here, including message dispatch.

## The [memory structure and garbage collector](Memory_structure-Garbage_collector.md) are described here.

## [Optimization opportunities](Optimizations.md) are described here.
## The [Just In Time compiler](JIT.md) is described here.

<!--
```chart
    type: bar
    labels: [Monday, Tuesday, Wednesday, Thursday, Friday]
    series: [[12, 5, 8, 8 , 5], [5, 8, 7, 9, 12]]
```

abc|def|ghi
---|---|---
qweqwe|dasdasd|

[SOmething](https://github.com/liamcain/obsidian-periodic-notes)
-->
