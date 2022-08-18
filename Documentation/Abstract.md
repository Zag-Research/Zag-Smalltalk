# Description of AST Smalltalk
### Abstract
AST-Smalltalk is a principle-based Smalltalk VM. "Principled" means that the only 3 operations are: message send, assignment, and return. There is no special-casing of methods like `ifTrue:ifFalse` or `whileTrue:`, although of course some methods are implemented by primitive methods.

The research question is, "Can this be made fast enough to be competitive?"

The basic idea is that methods are maintained in their AST form. An editor might provide a text-based version, and when importing/exporting to files we will generate a text version, but within the image the AST is the canonical form, and even the interpreter drives off the AST^[There is no byte code.].

Another goal is be be as compatible as possible with [Pharo](https://pharo.org) so that we can leverage most of the rich ecosystem!

The system is being developed on https://github.com/dvmason/AST-Smalltalk

## The [AST class structure](AST_Classes.md) is described here.

## The [interpreter](Execution.md) is described here, including message dispatch.

## The [memory structure](Mapping.md), [garbage collector](MemoryManagement.md), and [image format](ImageFormat.md) are described here.

## [Optimization opportunities](Optimizations.md) are described here.

## The [Just In Time compiler](JIT.md) is described here.

## [Experiments](Experiments.md)
<!--
| Start | Fibonacci |
| ----- | --------- |
| 1     | 1         |
| 2     | 1         |
| 3     | 2         |
| 4     | 3         |
| 5     | 5         |
| 6     | 8         |
| 7     | 13        |
| 8     | 21        |
<!-- TBLFM: @4$>..@>$>=(@-1+@-2) -->
<!-- TBLFM: @3$1..@>$1=(@-1+1) -->

<!--
```chart
    type: bar
    labels: [Monday, Tuesday, Wednesday, Thursday, Friday]
    series: [[12, 5, 8, 8 , 5], [5, 8, 7, 9, 12]]
```

|abc|def|ghi|
|---|---|---|
|qweqwe|dasdasdvxcv dfgd fdf d|wedwecsdf|
[SOmething](https://github.com/liamcain/obsidian-periodic-notes)

https://mermaid-js.github.io/mermaid/

```mermaid
gantt
    dateFormat  YYYY-MM-DD
    axisFormat  %m-%d
    title       Getting AST Smalltalk to viability
    excludes    weekends

    section Description
    %%Completed task        :done,   des1, 2021-01-06,2014-01-08
    Document Classes        :active, des2, 2021-03-01, 7d
    Interpreter             :        des3, after des2, 5d
    JIT                     :        des4, after des3, 5d

    Section Smalltalk
    AST classes             :active, si1, 2021-03-06, 5d
    Linearize               :        si2, after si1 des3, 3d
	Interpret               :        si3, after si2, 3d
	
    Section Rust
    Interpret               :active, si1, 2021-03-06, 4d
    GC                      :        si2, after si1 des3, 5d
	
	Section Codegen
    Basic codegen           :        cg1, after si2, 5d
	Basic JIT               :        cg2, after des4 cg1, 5d
	D-type opt              :        cg3, after cg1, 5d
	S-type opt              :        cg4, after cg3, 5d
	
	Section Pharo tools
	For benchmark           :        pti1, after si3, 5d
	
	Section Benchmarking
	Trivial                 :        bm1, after si2, 3d
	
	Section Write paper
	Basic outline           :active, wp1,20d
```
-->