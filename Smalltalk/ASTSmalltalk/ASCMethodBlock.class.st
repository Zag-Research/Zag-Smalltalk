"
I am an inline block, but I'm used to inline a Method

Returns from me are stack adjustment and branch to the return point
"
Class {
	#name : 'ASCMethodBlock',
	#superclass : 'ASCCompilableUnitBlock',
	#instVars : [
		'theMethod',
		'primitive',
		'returnBlocks',
		'selector'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCMethodBlock >> copyStackFrom: aBasicBlock [

	stack := aBasicBlock stack.
	self nameStack: theMethod argumentNames.
	^ stack
]

{ #category : 'as yet unclassified' }
ASCMethodBlock >> label [

	^ label ifNil: [
		  label := String streamContents: [ :s |
			           s
				           nextPutAll: super label;
				           nextPut: $_;
				           print: theMethod ] ]
]

{ #category : 'accessing' }
ASCMethodBlock >> primitive: anInteger [

	primitive := anInteger.

]

{ #category : 'accessing' }
ASCMethodBlock >> selector [

	^ selector
]

{ #category : 'accessing' }
ASCMethodBlock >> selector: aString [

	selector := aString.
	label := aString
]

{ #category : 'as yet unclassified' }
ASCMethodBlock >> theMethod: aCompiledMethod [

	theMethod := aCompiledMethod
]
