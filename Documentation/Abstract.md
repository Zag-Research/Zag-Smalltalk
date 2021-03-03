# Description of AST Smalltalk
### Abstract
AST-Smalltalk is a principle-based Smalltalk VM. The basic idea is that methods are maintained in their AST form. An editor might provide a text-based version, and when importing/exporting to files we will generate a text version, but within the image the AST is the canonical form, and even the interpreter drives off the AST^[There is no byte code.].

## The [[AST_Classes|AST class structure]] is described here.

## The [[Interpreter|interpreter]] is described here.

## The [[Method_dispatch|method dispatch]] is described here.

## The [[Memory_structure-Garbage_collector|memory structure and garbage collector]] are described here.

## [[Optimizations|Optimization opportunities]] are described here.