"
I have subclasses that are the first block created when compiling a method or block closure.

I will always only have a branch to an `ASCInlineBlock` which is where the method of block closure will be inlined.
"
Class {
	#name : 'ASCCompilableUnitBlock',
	#superclass : 'ASCBasicBlock',
	#instVars : [
		'primitive',
		'returnBlocks',
		'selector'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'initialization' }
ASCCompilableUnitBlock >> initialize [

	super initialize.
	returnBlocks := Dictionary new.
	method := self
]

{ #category : 'accessing' }
ASCCompilableUnitBlock >> primitive: anInteger [

	primitive := anInteger.

]

{ #category : 'accessing' }
ASCCompilableUnitBlock >> selector [

	^ selector
]

{ #category : 'accessing' }
ASCCompilableUnitBlock >> selector: aString [

	selector := aString.
	label := aString
]
